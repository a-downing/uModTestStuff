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

        static BasePlayer andrew = null;

        static void Print(string str) {
            Interface.Oxide.LogInfo(str, new object[]{});
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


        void OnServerInitialized() {
            
        }

        void Unload() {

        }
    }
}