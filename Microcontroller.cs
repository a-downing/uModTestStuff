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
    [Info("Microcontroller", "Andrew", "0.0.0")]
    public class Microcontroller : RustPlugin {
        string shortName = "electrical.random.switch.deployed";
        string prefab = "assets/prefabs/deployable/playerioents/gates/randswitch/electrical.random.switch.deployed.prefab";
        static int numChannels = 4;
        static BasePlayer andrew = null;
        bool ignoreSpawn = false;
        bool ignoreKill = false;
        ulong McuId = 0;
        Dictionary<ulong, McuComponent> McuComponents = new Dictionary<ulong, McuComponent>();
        Dictionary<uint, IOEntityMapper> ioEnts = new Dictionary<uint, IOEntityMapper>();

        struct IOEntityMapper {
            public ulong mcuId;
            public int index;
        }

        static void Print(string msg) {
            Interface.Oxide.LogInfo(msg, new object[]{});
        }

        class McuComponent {
            public ulong id;
            public IOEntity[] channels = new IOEntity[numChannels];
            public int[] inputEnergies = new int[numChannels];
            public int[] outputEnergies = new int[numChannels];

            public bool OnInputUpdate(int index, int inputAmount, int inputSlot) {
                IOEntity ioEnt = channels[index];
                IOEntity.IOSlot ioSlot = ioEnt.inputs[inputSlot];
                Print($"OnInputUpdate({inputAmount}, {inputSlot}) slot #{inputSlot} ioSlot.niceName: {ioSlot.niceName}");

                if(inputSlot != 0) {
                    return false;
                }

                inputEnergies[index] = inputAmount;

                // just pass through the power on each channel for now
                outputEnergies[index] = inputEnergies[index];

                return true;
            }

            public bool OnOutputUpdate(int index) {
                IOEntity ioEnt = channels[index];

                for(int i = 0; i < ioEnt.outputs.Length; i++) {
                    IOEntity.IOSlot ioSlot = ioEnt.outputs[i];
                    Print($"OnOutputUpdate slot #{i} ioSlot.niceName: {ioSlot.niceName}");

                    if(ioSlot.connectedTo.Get(true) != null) {
                        ioSlot.connectedTo.Get(true).UpdateFromInput(outputEnergies[index], ioSlot.connectedToSlot);
                    }
                }

                return true;
            }
        }

        object OnInputUpdate(IOEntity entity, int inputAmount, int inputSlot) {
            IOEntityMapper mapper;

            if(entity.net != null && ioEnts.TryGetValue(entity.net.ID, out mapper)) {
                McuComponent comp;

                if(McuComponents.TryGetValue(mapper.mcuId, out comp)) {
                    if(comp.OnInputUpdate(mapper.index, inputAmount, inputSlot)) {
                        return entity;
                    }
                }
            }

            return null;
        }

        object OnOutputUpdate(IOEntity entity) {
            IOEntityMapper mapper;

            if(entity.net != null && ioEnts.TryGetValue(entity.net.ID, out mapper)) {
                McuComponent comp;
                
                if(McuComponents.TryGetValue(mapper.mcuId, out comp)) {
                    if(comp.OnOutputUpdate(mapper.index)) {
                        return entity;
                    }
                }
            }

            return null;
        }

        void OnEntityKill(BaseNetworkable entity) {
            if(ignoreKill || entity.ShortPrefabName != shortName) {
                return;
            }

            IOEntity ioEnt = entity.GetComponent<IOEntity>();
            IOEntityMapper mapper;

            if(ioEnts.TryGetValue(ioEnt.net.ID, out mapper)) {
                McuComponent comp;
                
                if(McuComponents.TryGetValue(mapper.mcuId, out comp)) {
                    for(int i = 0; i < comp.channels.Length; i++) {
                        uint netId = comp.channels[i].net.ID;

                        if(i != mapper.index) {
                            ignoreKill = true;
                            comp.channels[i].Kill();
                            ignoreKill = false;
                        }

                        ioEnts.Remove(netId);
                    }

                    McuComponents.Remove(comp.id);
                }
            }
        }

        void OnEntitySpawned(BaseNetworkable entity) {
            if(ignoreSpawn || entity.ShortPrefabName != shortName) {
                return;
            }

            McuComponent comp = new McuComponent{
                id = McuId++
            };

            IOEntity ioEnt = entity.GetComponent<IOEntity>();
            comp.channels[0] = ioEnt;

            ioEnts.Add(ioEnt.net.ID, new IOEntityMapper {
                mcuId = comp.id,
                index = 0
            });

            Vector3 pos = entity.transform.position;
            ignoreSpawn = true;

            for(int i = 1; i < numChannels; i++) {
                BaseEntity copy = GameManager.server.CreateEntity(prefab, pos - entity.transform.right * (0.1f * i), entity.transform.rotation);
                copy.Spawn();

                ioEnt = copy.GetComponent<IOEntity>();
                comp.channels[i] = ioEnt;

                ioEnts.Add(ioEnt.net.ID, new IOEntityMapper {
                    mcuId = comp.id,
                    index = i
                });
            }

            ignoreSpawn = false;

            McuComponents.Add(comp.id, comp);
        }
    }
}