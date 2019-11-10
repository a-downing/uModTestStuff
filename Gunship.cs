using System;
using Oxide.Core;
using Oxide.Core.Configuration;
using Oxide.Core.Plugins;
using Oxide.Core.Libraries.Covalence;
using System.Linq;
using System.Collections.Generic;
using Newtonsoft.Json;
using System.Text.RegularExpressions;
using UnityEngine;

namespace Oxide.Plugins
{
    [Info("Gunship", "Andrew", "0.0.0")]
    public class Gunship : RustPlugin {
        Dictionary<ulong, GunshipPilot> pilots = new Dictionary<ulong, GunshipPilot>();
        ProjectileSimulator projectileSimulator = null;
        static BasePlayer andrew = null;
        static Benchmarker benchmarker = new Benchmarker();

        static void Print(string str) {
            Interface.Oxide.LogInfo(str, new object[]{});
        }

        class Benchmarker {
            class Benchmark {
                public string name;
                public float best;
                public float average;
                public float worst;
                public int samples;
            }

            Dictionary<string, Benchmark> benchmarks = new Dictionary<string, Benchmark>();

            public void Update(string name, float time) {
                Benchmark benchmark;
                if(!benchmarks.TryGetValue(name, out benchmark)) {
                    benchmarks.Add(name, new Benchmark {
                        name = name,
                        best = 0,
                        average = 0,
                        worst = 0,
                        samples = 1
                    });
                } else {
                    benchmark.samples++;
                    benchmark.best = Mathf.Min(benchmark.best, time);
                    benchmark.worst = (benchmark.samples < 10) ? 0 : Mathf.Max(benchmark.worst, time);
                    benchmark.average = (benchmark.samples < 10) ? 0 : benchmark.average - (benchmark.average / benchmark.samples) + (time / benchmark.samples);
                }
            }

            public void Report() {
                foreach(var pair in benchmarks) {
                    var benchmark = pair.Value;
                    Print($"benchmark: {benchmark.name} avg: {benchmark.average}, best: {benchmark.best}, worst: {benchmark.worst}");
                }
            }
        }

        class GunshipPilot {
            public EntityRef miniCopterRef;
            public BasePlayer pilot;
            public float lastPrimaryAttackTime = 0;
            public float lastSecondaryAttackTime = 0;
            public bool fireRight = true;
            public float gunElevation = 0;
            public float maxAbsGunElevation = 30.0f;
            public float primaryAttackPeriod = 1.0f / (500.0f / 60.0f);
            public float secondaryAttackPeriod = 1.0f / (60.0f / 60.0f);
        }

        class FiredProjectile {
            public EntityRef owner;
            public ItemDefinition itemDef;
            public ItemModProjectile itemMod;
            public Projectile projectilePrefab;
            public Projectile.Modifier projectileModifier = Projectile.Modifier.Default;
            public float firedTime;
            public float travelTime;
            public Vector3 position;
            public Vector3 velocity;
            public Vector3 initialPosition;
            public Vector3 initialVelocity;
        }

        public void Arrow(BasePlayer player, Vector3 from, Vector3 to, float headSize, Color color, float duration) {
            player.SendConsoleCommand("ddraw.arrow", duration, color, from, to, headSize);
        }

        public static void Sphere(BasePlayer player, Vector3 pos, float radius, Color color, float duration) {
            player.SendConsoleCommand("ddraw.sphere", duration, color, pos, radius);
        }

        public static void Line(BasePlayer player, Vector3 from, Vector3 to, Color color, float duration) {
            player.SendConsoleCommand("ddraw.line", duration, color, from, to);
        }

        class PerforatorRocket : ServerProjectile {
            public EntityRef target;
            public BaseEntity launcher;
            List<BaseCombatEntity> entList = new List<BaseCombatEntity>();
            HashSet<BaseEntity> ignoreSet = new HashSet<BaseEntity>();

            public void AddIgnore(BaseEntity ent) {
                ignoreSet.Add(ent);
            }

            void FixedUpdate() {
                base.DoMovement();
                var launcherDist = (launcher.transform.position - transform.position).magnitude;

                if(!target.IsSet()) {
                    Vis.Entities<BaseCombatEntity>(transform.position, 20, entList, 8192 | (1 << 11) | (1 << 17), QueryTriggerInteraction.Ignore);
                    float best = -1.0f;
                    BaseEntity bestTarget = null;

                    foreach(var ent in entList) {
                        if(ignoreSet.Contains(ent)) {
                            continue;
                        }

                        var dir = ent.transform.position - transform.position;
                        var dot = Vector3.Dot(_currentVelocity, dir);

                        if(dot > best) {
                            best = dot;
                            bestTarget = ent;
                        }

                        //Print($"ent.name: {ent.name}");
                    }

                    if(bestTarget && launcherDist > 5.0f) {
                        target.Set(bestTarget);
                    }
                } else {
                    var hit = new RaycastHit();
                    hit.normal = Vector3.zero;
                    SendMessage("ProjectileImpact", hit);
                    Perforate();
                }
            }

            void Perforate() {
                Vector3 targetDir = target.Get(true).transform.position - transform.position;
                targetDir.Normalize();

                for(int i = 0; i < 10; i++) {
                    var ent = GameManager.server.CreateEntity("assets/prefabs/npc/m2bradley/maincannonshell.prefab", transform.position);
                    var proj = ent.GetComponent<ServerProjectile>();
                    var timedEx = ent.GetComponent<TimedExplosive>();
        
                    proj.InitializeVelocity(targetDir * 100 + UnityEngine.Random.insideUnitSphere * 20);
                    proj.gravityModifier = 1;
                    proj.drag = 1;
        
                    timedEx.damageTypes.Clear();
                
                    timedEx.damageTypes.Add(new Rust.DamageTypeEntry {
                        type = Rust.DamageType.Bullet,
                        amount = 100.0f
                    });
        
                    timedEx.damageTypes.Add(new Rust.DamageTypeEntry {
                        type = Rust.DamageType.Explosion,
                        amount = 10.0f
                    });
        
                    ent.Spawn();
                }
            }
        }

        class ProjectileSimulator : MonoBehaviour {
            List<FiredProjectile> firedProjectiles = new List<FiredProjectile>();
            bool toggle = true;
            uint counter = 0;
            Item item = ItemManager.CreateByName("ammo.rifle.incendiary");

            public void Init() {
                InvokeRepeating(nameof(Simulate), 0.03125f, 0.03125f);
            }

            public void Add(FiredProjectile firedProjectile) {
                firedProjectiles.Add(firedProjectile);
            }

            void Simulate() {
                var startTime = Time.realtimeSinceStartup;

                for(int i = firedProjectiles.Count - 1; i >= 0; i--) {
                    var firedProjectile = firedProjectiles.ElementAt(i);
                    bool status = SimulateProjectile(firedProjectile, 0.03125f);
                    
                    if(!status) {
                        firedProjectiles.RemoveAt(i);
                    }
                }

                var elapsedTime = Time.realtimeSinceStartup - startTime;
                benchmarker.Update("Simulate", elapsedTime);

                if(counter % Mathf.RoundToInt(1.0f / 0.03125f) == 0) {
                    benchmarker.Report();
                    Print($"firedProjectiles.Count: {firedProjectiles.Count}");
                }

                counter++;
            }

            bool SimulateProjectile(FiredProjectile firedProjectile, float dt) {
                if(firedProjectile.travelTime == 0) {
                    dt = Time.realtimeSinceStartup - firedProjectile.firedTime;
                } else if(firedProjectile.travelTime > 8.0f) {
                    return false;
                }

                Vector3 gravity = UnityEngine.Physics.gravity * firedProjectile.projectilePrefab.gravityModifier;
                Vector3 oldPosition = firedProjectile.position;
                firedProjectile.position += firedProjectile.velocity * dt;
                firedProjectile.velocity += gravity * dt;
                firedProjectile.velocity -= firedProjectile.velocity * firedProjectile.projectilePrefab.drag * dt;
                firedProjectile.travelTime += dt;

                //Line(andrew, oldPosition, firedProjectile.position, (toggle) ? Color.red : Color.blue, 1);
                toggle = !toggle;

                Vector3 vec = firedProjectile.position - oldPosition;
                var list = new List<RaycastHit>();
                GamePhysics.TraceAll(new Ray(oldPosition, vec), 0f, list, vec.magnitude, 1219701521, QueryTriggerInteraction.Ignore);

                bool didntHit = true;

                foreach(var hit in list) {
                    BaseEntity ent = hit.GetEntity();

                    didntHit = false;

                    HitInfo hitInfo = new HitInfo();
                    hitInfo.ProjectilePrefab = firedProjectile.projectilePrefab;
                    hitInfo.Initiator = firedProjectile.owner.Get(true);
                    hitInfo.HitEntity = ent;
                    hitInfo.HitPositionWorld = hit.point;
                    hitInfo.HitNormalWorld = hit.normal;
                    hitInfo.PointStart = firedProjectile.initialPosition;
                    hitInfo.ProjectileDistance = (firedProjectile.position - firedProjectile.initialPosition).magnitude;
                    
                    hitInfo.ProjectilePrefab.CalculateDamage(hitInfo, firedProjectile.projectileModifier, 1);
                    firedProjectile.itemMod.ServerProjectileHit(hitInfo);
                    Effect.server.ImpactEffect(hitInfo);

                    if(ent == null) {
                        return false;
                    }

                    ent.OnAttacked(hitInfo);

                    if(ent.ShouldBlockProjectiles()) {
                        break;
                    }
                }

                return didntHit;
            }

            public void FireProjectile(BasePlayer player, Vector3 position, Vector3 velocity) {
                //var item = ItemManager.CreateByName("ammo.rifle.incendiary");
                var itemModProjectile = item.info.GetComponent<ItemModProjectile>();
                var projectile = itemModProjectile.projectileObject.Get().GetComponent<Projectile>();

                //Puts($"itemModProjectile.projectileVelocity: {itemModProjectile.projectileVelocity}");

                FiredProjectile firedProjectile = new FiredProjectile
                {
                    itemDef = item.info,
                    itemMod = itemModProjectile,
                    projectilePrefab = projectile,
                    firedTime = UnityEngine.Time.realtimeSinceStartup,
                    travelTime = 0f,
                    position = position,
                    velocity = velocity,
                    initialPosition = position,
                    initialVelocity = velocity
                };

                Add(firedProjectile);

                Effect effect = new Effect();
                effect.Clear();
                effect.Init(global::Effect.Type.Projectile, position, velocity, null);
                effect.scale = 2;
                effect.pooledString = itemModProjectile.projectileObject.resourcePath;
                effect.number = 1;
                EffectNetwork.Send(effect);
            }
        }

        void OnServerInitialized() {
            var go = new GameObject();
            go.name = "projectilesimulator";
            projectileSimulator = go.AddComponent<ProjectileSimulator>();
            projectileSimulator.Init();
        }

        void Unload() {
            GameObject.Destroy(projectileSimulator);

            /*var list = GameObject.FindObjectsOfType<MonoBehaviour>();
            foreach(var ent in list) {
                if(ent.name == "projectilesimulator") {
                    GameObject.Destroy(ent);
                }
            }*/
        }

        void OnEntityMounted(BaseMountable entity, BasePlayer player) {
            var miniCopter = entity.GetParentEntity() as MiniCopter;

            if(miniCopter && miniCopter.ShortPrefabName == "minicopter.entity") {
                andrew = player;

                if(!pilots.ContainsKey(player.userID)) {
                    var pilot = new GunshipPilot();
                    pilot.pilot = player;
                    pilot.miniCopterRef.Set(miniCopter);

                    pilots.Add(player.userID, pilot);
                }
            }
        }

        void OnEntityDismounted(BaseMountable entity, BasePlayer player) {
            if(pilots.ContainsKey(player.userID)) {
                pilots.Remove(player.userID);
            }
        }

        [ConsoleCommand("gunship.gunelevation")]
        void AdjustGunAngle(ConsoleSystem.Arg arg) {
            BasePlayer player = arg.Player();

            if(!player) {
                Puts("must be run as player");
                return;
            }
            
            GunshipPilot pilot;
           
            if(pilots.TryGetValue(player.userID, out pilot)) {
                if(arg.Args.Length >= 1) {
                    float dir = 0;

                    if(arg.Args[0] == "+") {
                        dir = 1;
                    } else if(arg.Args[0] == "-") {
                        dir = -1;
                    } else {
                        return;
                    }

                    pilot.gunElevation += dir;

                    if(Mathf.Abs(pilot.gunElevation) > pilot.maxAbsGunElevation) {
                        pilot.gunElevation = pilot.maxAbsGunElevation * Mathf.Sign(pilot.gunElevation);
                    }

                    //Vector3 vec = Quaternion.AngleAxis(pilot.gunElevation, pilot.pilot.eyes.HeadRight()) * pilot.pilot.eyes.HeadForward();
                    //Arrow(andrew, pilot.pilot.eyes.position, pilot.pilot.eyes.position + vec * 5, 0.1f, Color.green, 0.25f);
                }
            }
        }

        void OnPlayerInput(BasePlayer player, InputState input) {
            GunshipPilot pilot;

            if(pilots.TryGetValue(player.userID, out pilot)) {
                MiniCopter miniCopter = pilot.miniCopterRef.Get(true) as MiniCopter;

                if(!miniCopter) {
                    if(pilots.ContainsKey(player.userID)) {
                        pilots.Remove(player.userID);
                    }

                    return;
                }

                if((input.current.buttons & (int)BUTTON.FIRE_SECONDARY) != 0) {
                    if(Time.time > pilot.lastSecondaryAttackTime + pilot.secondaryAttackPeriod) {
                        Vector3 launchPos = miniCopter.transform.position + (miniCopter.transform.rotation * new Vector3(-1.0f, 0, 1.0f));
                        BaseEntity rocket = GameManager.server.CreateEntity("assets/prefabs/ammo/rocket/rocket_basic.prefab", launchPos, new Quaternion(), true);
                        TimedExplosive rocketExplosion = rocket.GetComponent<TimedExplosive>();

                        ServerProjectile proj = rocket.GetComponent<ServerProjectile>();
                        GameObject.Destroy(proj);
                        var rocketProjectile = rocket.gameObject.AddComponent<PerforatorRocket>() as PerforatorRocket;

                        rocketProjectile.gravityModifier = 0.5f;
                        rocketProjectile.AddIgnore(miniCopter);
                        rocketProjectile.launcher = miniCopter;

                        rocketExplosion.timerAmountMin = 60;
                        rocketExplosion.timerAmountMax = 60;

                        for (int i = 0; i < rocketExplosion.damageTypes.Count; i++) {
                            rocketExplosion.damageTypes[i].amount *= 100;
                        }

                        rocketProjectile.InitializeVelocity(miniCopter.transform.forward * 50 + miniCopter.rigidBody.velocity);
                        rocket.Spawn();
                        pilot.lastSecondaryAttackTime = Time.time;
                    }
                }

                if((input.current.buttons & (int)BUTTON.FIRE_PRIMARY) != 0) {
                    if(Time.time > pilot.lastPrimaryAttackTime + pilot.primaryAttackPeriod) {
                        pilot.lastPrimaryAttackTime = Time.time;
                        Vector3 muzzleOffset = new Vector3(0, 0.6f, 0.5f);

                        if(pilot.fireRight) {
                            muzzleOffset.x += 0.65f;
                        } else {
                            muzzleOffset.x += -0.65f;
                        }

                        pilot.fireRight = !pilot.fireRight;

                        Vector3 forward = miniCopter.transform.forward;
                        forward = Quaternion.AngleAxis(pilot.gunElevation, miniCopter.transform.right) * forward;
                        Vector3 velocity = forward * 200;
                        velocity += UnityEngine.Random.insideUnitSphere * 2;
                        velocity += miniCopter.rigidBody.velocity;
                        Vector3 position = miniCopter.transform.position + (miniCopter.transform.rotation * muzzleOffset);

                        Effect.server.Run("assets/prefabs/npc/m2bradley/effects/maincannonattack.prefab", position);
                        projectileSimulator.FireProjectile(player, position, velocity);
                        //Effect.server.Run("assets/prefabs/weapons/ak47u/effects/attack.prefab", position);
                        //muzzleOffset.y = 0;
                        //miniCopter.rigidBody.AddForceAtPosition(-velocity.normalized * 25, miniCopter.transform.position + (miniCopter.transform.rotation * muzzleOffset), ForceMode.Impulse);
                    }
                }
            }
        }
    }
}