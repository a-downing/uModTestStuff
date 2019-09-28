using System;
using Oxide.Core;
using UnityEngine;

namespace Oxide.Plugins
{
    [Info("TimerTest", "Andrew", "0.0.0")]
    public class TimerTest : RustPlugin {
        TimerBehavior timerBehavior = null;

        class TimerBehavior : MonoBehaviour {
            public class Timer {
                public float timeStarted;
                public Coroutine coroutine;
                public Action action;
            }

            public Timer StartTimer(Action action, float delay) {
                return new Timer {
                    timeStarted = Time.realtimeSinceStartup,
                    action = action,
                    coroutine = StartCoroutine(ExecuteAfterTime(action, delay))
                };
            }

            public void StopTimer(Timer timer) {
               StopCoroutine(timer.coroutine);
            }

            System.Collections.IEnumerator ExecuteAfterTime(Action action, float delay) {
                yield return new WaitForSeconds(delay);
                action();
            }
        }

        void OnServerInitialized() {
            var go = new GameObject();
            go.name = "timerbehavior";
            timerBehavior = go.AddComponent<TimerBehavior>();

            var timer1 = timerBehavior.StartTimer(() => {
                Puts("timer1");
            }, 3.0f);

            var timer2 = timerBehavior.StartTimer(() => {
                Puts("timer2");
            }, 4.0f);

            var timer3 = timerBehavior.StartTimer(() => {
                Puts("timer3");
            }, 5.0f);

            var timer4 = timerBehavior.StartTimer(() => {
                Puts("timer4");

                var timer5 = timerBehavior.StartTimer(() => {
                    Puts("timer5");
                }, 5.0f);
            }, 6.0f);

            timerBehavior.StopTimer(timer1);
        }

        void Unload() {
            GameObject.Destroy(timerBehavior);
        }
    }
}
