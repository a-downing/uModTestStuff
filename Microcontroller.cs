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

using System.Runtime.InteropServices;

namespace Oxide.Plugins
{
    [Info("Microcontroller", "Andrew", "0.0.0")]
    public class Microcontroller : RustPlugin {
        static ConfigData config;
        static Microcontroller plugin = null;
        static Compiler compiler = null;
        string shortName = "electrical.random.switch.deployed";
        string prefab = "assets/prefabs/deployable/playerioents/gates/randswitch/electrical.random.switch.deployed.prefab";
        static int numChannels = 4;
        bool ignoreSpawn = false;
        ulong McuId = 0;
        Dictionary<ulong, McuComponent> McuComponents = new Dictionary<ulong, McuComponent>();
        Dictionary<uint, IOEntityMapper> ioEnts = new Dictionary<uint, IOEntityMapper>();
        McuManager manager = null;
        const int maxAvgCharsPerLine = 32;
        
        //testing
        BetterCPU betterCPU = new BetterCPU();

        class BetterCPU {
            Union[] memory = null;
            uint[] instructions = null;
            int[] args = new int[8];
            int pic = 0;
            int bsp = 0;

            [StructLayout(LayoutKind.Explicit)]
            public struct Union {
                [FieldOffset(0)]
                public int Int;
                [FieldOffset(0)]
                public uint Uint;
                [FieldOffset(0)]
                public float Float;
            }

            public enum OpcodeType : byte {
                CORE = 0
            }

            public enum Opcode : byte {
                NOP = 0,
                MOV,
                JMP,
                CALL,
                PUSH,
                POP,
                RSHIFT,
                LSHIFT
            }

            // I dunno
            public enum ArgType : byte {
                NONE = 0,
                IMMEDIATE,
                INDIRECT_0,
                INDIRECT_1,
            }

            public enum Status {
                SUCCESS,
                OUT_OF_INSTRUCTIONS,
                MISSING_INSTRUCTION,
                BAD_INSTRUCTION,
                SEGFAULT
            }

            Opcode GetOpcode(uint inst) {
                return (Opcode)((inst & 0xFF000000) >> 24);
            }

            int GetNumArgs(uint inst) {
                return (int)((inst & 0x00FF0000) >> 16);
            }

            OpcodeType GetOpcodeType(uint inst) {
                return (OpcodeType)((inst & 0x0000FF00) >> 8);
            }

            public uint MakeInstruction(Opcode opcode, int numArgs, OpcodeType opcodeType) {
                return (uint)opcode << 24 | (uint)numArgs << 16 | (uint)opcodeType << 8;
            }

            public void LoadProgram(uint[] instructions, Union[] memory, int bsp, int pic = 0) {
                this.instructions = new uint[instructions.Length];
                instructions.CopyTo(this.instructions, 0);
                this.memory = new Union[memory.Length];
                memory.CopyTo(this.memory, 0);
                this.pic = pic;
                this.bsp = bsp;
            }

            void SetMem(int index, Union value) {
                memory[index] = value;
            }

            Union GetMem(int index) {
                return memory[index];
            }

            public bool Cycle(out Status status) {
                if(pic >= instructions.Length) {
                    status = Status.OUT_OF_INSTRUCTIONS;
                    return false;
                }

                uint inst = instructions[pic++];
                Opcode opcode = GetOpcode(inst);
                int numArgs = GetNumArgs(inst);
                OpcodeType instType = GetOpcodeType(inst);

                if(numArgs >= args.Length) {
                    status = Status.BAD_INSTRUCTION;
                    return false;
                }

                for(int i = 0; i < numArgs; i++) {
                    args[i] = (int)instructions[pic++];

                    /*if(args[i] >= memory.Length) {
                        status = Status.SEGFAULT;
                        return false;
                    }*/
                }

                switch(instType) {
                    case OpcodeType.CORE:
                        goto core;
                }

                core:
                switch(opcode) {
                    case Opcode.MOV:
                        SetMem(args[0], GetMem(args[1]));
                        break;
                    case Opcode.JMP:
                        pic = memory[args[0]].Int;
                        break;
                    case Opcode.CALL:
                        memory[bsp++] = new Union{ Int = pic };
                        pic = memory[args[0]].Int;
                        break;
                    case Opcode.PUSH:
                        memory[bsp++] = memory[args[0]];
                        break;
                    case Opcode.POP:
                        memory[args[0]] = memory[bsp--];
                        break;
                    case Opcode.RSHIFT:
                        memory[args[0]].Uint = memory[args[0]].Uint >> memory[args[1]].Int;
                        break;
                    case Opcode.LSHIFT:
                        memory[args[0]].Uint = memory[args[0]].Uint << memory[args[1]].Int;
                        break;
                    default:
                        status = Status.MISSING_INSTRUCTION;
                        return false;
                }

                status = Status.SUCCESS;
                return true;
            }
        }

        struct IOEntityMapper {
            public ulong mcuId;
            public int index;
        }

        static void Print(string msg) {
            Interface.Oxide.LogInfo(msg, new object[]{});
        }

        void Init() {
            config = Config.ReadObject<ConfigData>();
            plugin = this;
            compiler = new Compiler();

            //Puts($"new GUID: {Guid.NewGuid()}");
        }

        void OnServerInitialized() {
            var go = new GameObject(McuManager.Guid);
            manager = go.AddComponent<McuManager>();
            manager.Init(this);

            BetterCPU.Union[] data = new BetterCPU.Union[] {
                new BetterCPU.Union{ Uint = 0 },
                new BetterCPU.Union{ Uint = 33 },
                new BetterCPU.Union{ Uint = 42 },
                new BetterCPU.Union{ Uint = 8 },
            };

            BetterCPU.Union[] memory = new BetterCPU.Union[1024];
            data.CopyTo(memory, 0);

            uint[] instructions = new uint[] {
                betterCPU.MakeInstruction(BetterCPU.Opcode.MOV, 2, BetterCPU.OpcodeType.CORE), 1, 2,
                betterCPU.MakeInstruction(BetterCPU.Opcode.PUSH, 1, BetterCPU.OpcodeType.CORE) , 1,
                betterCPU.MakeInstruction(BetterCPU.Opcode.POP, 1, BetterCPU.OpcodeType.CORE), 2,
                betterCPU.MakeInstruction(BetterCPU.Opcode.RSHIFT, 2, BetterCPU.OpcodeType.CORE), 2, 3,
                betterCPU.MakeInstruction(BetterCPU.Opcode.LSHIFT, 2, BetterCPU.OpcodeType.CORE), 2, 3,
                betterCPU.MakeInstruction(BetterCPU.Opcode.JMP, 1, BetterCPU.OpcodeType.CORE) , 0
            };

            betterCPU.LoadProgram(instructions, memory, data.Length);
            float startTime = Time.realtimeSinceStartup;
            int numInstructions = 10000000;

            for(int i = 0; i < numInstructions; i++) {
                BetterCPU.Status status;

                if(!betterCPU.Cycle(out status)) {
                    Print($"cpu error: {status.ToString()}");
                    break;
                }
            }

            float elapsedTime = Time.realtimeSinceStartup - startTime;
            Print($"{numInstructions} in {elapsedTime}s ({numInstructions / elapsedTime} instructions/s)");
        }

        void Unload() {
            if(manager) {
                GameObject.Destroy(manager);
            }

            plugin = null;
            config = null;
            compiler = null;
        }

        void OnEntityKill(BaseNetworkable entity) {
            if(entity.ShortPrefabName != shortName) {
                return;
            }

            IOEntity ioEnt = entity.GetComponent<IOEntity>();
            IOEntityMapper mapper;

            if(ioEnts.TryGetValue(ioEnt.net.ID, out mapper)) {
                McuComponent comp;
                
                if(McuComponents.TryGetValue(mapper.mcuId, out comp)) {
                    for(int i = 0; i < comp.channels.Length; i++) {
                        ioEnts.Remove(comp.channels[i].net.ID);

                        if(i != mapper.index) {
                            comp.channels[i].Kill();
                        }
                    }

                    comp.stash.Kill();
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

            Vector3 offset = pos - entity.transform.right * (0.1f * numChannels);
            offset -= entity.transform.right * 0.2f;
            Quaternion rotation = entity.transform.rotation;
            Vector3 right = rotation * Vector3.right;
            Quaternion mod = Quaternion.AngleAxis(90, right);
            rotation = mod * rotation;

            comp.stash = GameManager.server.CreateEntity("assets/prefabs/deployable/small stash/small_stash_deployed.prefab", offset, rotation);
            comp.stash.Spawn();

            StorageContainer storage = comp.stash.GetComponent<StorageContainer>();

            storage.inventory.onItemAddedRemoved = new Action<Item, bool>((Item item, bool added) => {
                comp.OnItemAddedOrRemoved(storage, item, added);
                storage.OnItemAddedOrRemoved(item, added);
            });

            ignoreSpawn = false;

            /*bool success = compiler.Compile(@"
            jmp main

            isr input
                num index; pop index
                num value; getin value index
                print index index
                print value value
                setout index value
                sei
                ret

            label main
                jmp main
            ");

            if(success) {
                comp.cpu.LoadInstructions(compiler.instructions);
            } else {
                foreach(var error in compiler.errors) {
                    Puts(error);
                }
            }*/

            McuComponents.Add(comp.id, comp);
        }

        protected override void LoadDefaultMessages() {
            lang.RegisterMessages(new Dictionary<string, string> {
                ["var_already_declared"] = "line {0}: variable already declared ({1})",
                ["invalid_label"] = "line {0}: invalid label ({1})",
                ["var_not_declared"] = "line {0}: variable has not been declared ({1})",
                ["incompat_arg_type"] = "line {0}: incompatible argument type ({1}:{2}) for instruction ({3})",
                ["invalid_arg"] = "line {0}: invalid argument ({1}) for ({2}) spec: {2} {3}",
                ["invalid_instruction"] = "line {0}: invalid instruction ({1})",
                ["wrong_num_args"] = "line {0}: wrong number of arguments for ({1}) spec: {1} {2}",
                ["over_char_limit"] = "max program length is {0} characters",
                ["over_instruction_limit"] = "max program length is {0} instructions",
                ["compiler_error"] = "[error] {0}",
                ["no_prefab"] = "Error creating prefab \"{0}\""
            }, this);
        }

        protected override void LoadDefaultConfig() {
            var config = new ConfigData {
                maxProgramInstructions = 512,
                maxInstructionsPerCycle = 32,
                CPUFreq = 10
            };

            Config.WriteObject(config, true);
        }

        class ConfigData {
            [JsonProperty(PropertyName = "maxProgramInstructions")]
            public int maxProgramInstructions;
            [JsonProperty(PropertyName = "maxInstructionsPerCycle")]
            public int maxInstructionsPerCycle;
            [JsonProperty(PropertyName = "CPUFreq")]
            public float CPUFreq;
        }

        class McuManager : MonoBehaviour {
            public const string Guid = "d7106397-4efc-44c6-b541-1312fb455cde";
            public Microcontroller plugin = null;

            public void Init(Microcontroller plugin) {
                this.plugin = plugin;
                float period = 1.0f / config.CPUFreq;
                InvokeRepeating(nameof(CycleCPUs), period, period);
            }

            void CycleCPUs() {
                if(plugin.McuComponents.Count == 0) {
                    return;
                }

                foreach(var value in plugin.McuComponents) {
                    var comp = value.Value;
                    comp.CycleCPU();
                }
            }

            void Reset() {
                foreach(var comp in plugin.McuComponents) {
                    comp.Value.Reset();
                }
            }
        }

        class McuComponent {
            public ulong id;
            public IOEntity[] channels = new IOEntity[numChannels];
            public int[] inputEnergies = new int[numChannels];
            public int[] outputEnergies = new int[numChannels];
            public CPU cpu = new CPU();
            Compiler.Instruction currentInstruction = null;
            public Flag flags;
            public BaseEntity stash = null;

            [Flags]
            public enum Flag {
                ExecutingInstruction = 1 << 1
            }

            bool HasFlag(Flag f) {
                return (this.flags & f) == f;
            }

            public void SetFlag(Flag f, bool set) {
                if(set) {
                    this.flags |= f;
                    return;
                }

                this.flags &= ~f;
            }

            public int TotalInputEnergy() {
                int sum = 0;

                for(int i = 0; i < inputEnergies.Length; i++) {
                    sum += inputEnergies[i];
                }

                return sum;
            }

            public void UpdateOutputEnergies(int[] energies) {
                int availableEnergy = TotalInputEnergy();
                int used = 0;

                for(int i = 0; i < energies.Length; i++) {
                    int energy = Mathf.Min(availableEnergy - used, energies[i]);
                    used = Mathf.Min(availableEnergy, used + energies[i]);

                    if(energy != outputEnergies[i]) {
                        UpdateSingleOutput(i, 0, energy);
                        outputEnergies[i] = energy;
                    }
                }
            }

            public bool OnInputUpdate(int index, int inputAmount, int inputSlot) {
                IOEntity ioEnt = channels[index];
                IOEntity.IOSlot ioSlot = ioEnt.inputs[inputSlot];
                Print($"OnInputUpdate({inputAmount}, {inputSlot}) slot #{inputSlot} ioSlot.niceName: {ioSlot.niceName}");

                if(inputSlot != 0) {
                    return false;
                }

                if(inputAmount != inputEnergies[index]) {
                    inputEnergies[index] = inputAmount;
                    ioEnt.IOStateChanged(inputAmount, inputSlot);
                    cpu.Interrupt("input", new float[]{index});
                }

                int totalEnergy = TotalInputEnergy();

                for(int i = 0; i < channels.Length; i++) {
                    if(totalEnergy > 0) {
                        channels[i].SetFlag(global::BaseEntity.Flags.Reserved8, true);
                    } else {
                        channels[i].SetFlag(global::BaseEntity.Flags.Reserved8, false);
                    }
                }

                return true;
            }

            public void UpdateSingleOutput(int ioEntindex, int outputIndex, int energy) {
                IOEntity ioEnt = channels[ioEntindex];
                IOEntity.IOSlot ioSlot = ioEnt.outputs[outputIndex];
                IOEntity target = ioSlot.connectedTo.Get(true);

                Print($"UpdateSingleOutput({ioEntindex}, {outputIndex}, {energy}) ioSlot.niceName: {ioSlot.niceName}");

                if(target) {
                    target.UpdateFromInput(energy, ioSlot.connectedToSlot);
                    
                    if(energy > 0) {
                        ioEnt.SetFlag(global::BaseEntity.Flags.On, true);
                    } else {
                        ioEnt.SetFlag(global::BaseEntity.Flags.On, false);
                    }
                }
            }

            public bool OnOutputUpdate(int index) {
                IOEntity ioEnt = channels[index];

                for(int i = 0; i < ioEnt.outputs.Length; i++) {
                    UpdateSingleOutput(index, i, outputEnergies[index]);
                }

                return true;
            }

            public void OnItemAddedOrRemoved(StorageContainer storage, Item item, bool added) {
                string message;

                if(item.info.shortname == "note" && item.text != null) {
                    var maxProgramCharLength = config.maxProgramInstructions * maxAvgCharsPerLine;

                    if(item.text.Length > maxProgramCharLength) {
                        message = string.Format(plugin.lang.GetMessage("over_char_limit", plugin), maxProgramCharLength);
                        message = string.Format(plugin.lang.GetMessage("compiler_error", plugin), message);
                        item.text += "\n" + message;
                        return;
                    }

                    var lines = item.text.ToLower().Split(new[] {"\r\n", "\r", "\n", ";"}, StringSplitOptions.RemoveEmptyEntries);

                    if(lines.Length >= 1) {
                        if(lines[0] == "#asm") {
                            if(added) {
                                if(lines.Length < config.maxProgramInstructions) {
                                    Reset();
                                    bool success = compiler.Compile(item.text);

                                    if(!success) { 
                                        item.text += "\n" + String.Join("\n", compiler.errors.Select(x => {
                                            return string.Format(plugin.lang.GetMessage("compiler_error", plugin), x);
                                        }));
                                    } else {
                                        Print("loaded instructions");
                                        cpu.LoadInstructions(compiler.instructions);
                                    }
                                } else {
                                    message = string.Format(plugin.lang.GetMessage("over_instruction_limit", plugin), config.maxProgramInstructions);
                                    message = string.Format(plugin.lang.GetMessage("compiler_error", plugin), message);
                                    item.text += "\n" + message;
                                }
                            } else {
                                Reset();
                            }
                        }
                    }
                }
            }

            public void Reset() {
                currentInstruction = null;
                cpu.Reset();
            }

            public void CycleCPU() {
                if(TotalInputEnergy() == 0) {
                    return;
                }

                /*if(this.currentInstruction != null && HasFlag(Flag.ExecutingInstruction)) {
                    switch(this.currentInstruction.name) {
                        
                    }
                }*/

                if(cpu.HandlePendingInterrupt()) {
                    SetFlag(Flag.ExecutingInstruction, false);
                }

                int numInstructionsExecuted = 0;
                string failReason;
                while(numInstructionsExecuted < config.maxInstructionsPerCycle && !HasFlag(Flag.ExecutingInstruction)) {
                    if(!cpu.Cycle(out this.currentInstruction, out failReason)) {
                        Print(failReason);
                        Reset();
                        return;
                    }

                    numInstructionsExecuted++;

                    if(currentInstruction.name != "label" && currentInstruction.name != "jmp") {
                        //Print(currentInstruction.name);
                    }

                    switch(this.currentInstruction.name) {
                        case "getin":
                            string destVar = this.currentInstruction.args[0].stringValue;
                            int index = this.currentInstruction.args[1].intValue;

                            if(index >= 0 && index < inputEnergies.Length) {
                                cpu.WriteVariable(destVar, inputEnergies[index]);
                            }

                            break;
                        case "getout":
                            destVar = this.currentInstruction.args[0].stringValue;
                            index = this.currentInstruction.args[1].intValue;

                            if(index >= 0 && index < outputEnergies.Length) {
                                cpu.WriteVariable(destVar, outputEnergies[index]);
                            }

                            break;
                        case "setout":
                            index = this.currentInstruction.args[0].intValue;
                            int value = this.currentInstruction.args[1].intValue;
                            int[] newOutputEnergies = new int[outputEnergies.Length];
                            outputEnergies.CopyTo(newOutputEnergies, 0);

                            if(index >= 0 && index < outputEnergies.Length) {
                                newOutputEnergies[index] = value;
                                UpdateOutputEnergies(newOutputEnergies);
                            }

                            break;
                        case "gettime":
                            destVar = this.currentInstruction.args[0].stringValue;
                            cpu.WriteVariable(destVar, Time.time);

                            break;
                        case "print":
                            string arg1 = this.currentInstruction.args[0].stringValue;
                            float arg2 = this.currentInstruction.args[1].floatValue;
                            Print($"print {arg1} {arg2}");

                            break;
                    }

                    //if(cpu.HandlePendingInterrupt()) {
                    //    SetFlag(Flag.ExecutingInstruction, false);
                    //}
                }
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

        class CPU {
            List<Compiler.Instruction> instructions = new List<Compiler.Instruction>();
            public Dictionary<string, int> isrs = new Dictionary<string, int>();
            public Queue<string> interrupts = new Queue<string>();
            public List<int> picStack = new List<int>();
            int pic = 0;
            bool abort = false;
            string abortReason = null;
            Dictionary<string, Compiler.Instruction.Argument> numVariables = new Dictionary<string, Compiler.Instruction.Argument>();
            public List<float> stack = new List<float>();
            public int maxStackSize = 64;
            bool interruptsEnabled = true;

            public void Reset() {
                pic = 0;
                interrupts.Clear();
                picStack.Clear();
                numVariables.Clear();
            }

            public void LoadInstructions(List<Compiler.Instruction> instructions) {
                this.instructions = instructions;
                isrs.Clear();
                interrupts.Clear();
                picStack.Clear();
                numVariables.Clear();
                stack.Clear();
                pic = 0;

                for(int i = 0; i < instructions.Count; i++) {
                    var instr = instructions[i];

                    if(instr.name == "isr") {
                        isrs.Add(instr.args[0].stringValue, i);
                    } else if(instr.name == "num") {
                        numVariables.Add(instr.args[0].stringValue, instr.args[0]);
                    }
                }
            }

            public void Jump(int addr) {
                pic = addr;
            }

            public void Call(int addr) {
                picStack.Add(pic);
                pic = addr;
            }

            public bool Ret() {
                if(picStack.Count == 0) {
                    return false;
                }

                Jump(picStack[picStack.Count - 1]);
                picStack.RemoveAt(picStack.Count - 1);

                return true;
            }

            public void Interrupt(string name, float[] args = null) {
                if(isrs.ContainsKey(name)) {
                    if(args != null) {
                        for(int i = 0; i < args.Length; i++) {
                            stack.Add(args[i]);
                        }
                    }

                    interrupts.Enqueue(name);
                }
            }

            public bool WriteVariable(string name, float value) {
                Compiler.Instruction.Argument arg;
                if(numVariables.TryGetValue(name, out arg)) {
                    arg.floatValue = value;
                    arg.intValue = (int)value;
                    return true;
                }

                return false;
            }

            public bool HandlePendingInterrupt() {
                if(!interruptsEnabled || interrupts.Count == 0) {
                    return false;
                }

                interruptsEnabled = false;

                var isr = interrupts.Dequeue();
                int addr;
                isrs.TryGetValue(isr, out addr);
                Call(addr);

                return true;
            }

            public void Abort(string reason) {
                abort = true;
                abortReason = reason;
            }

            public bool Cycle(out Compiler.Instruction instr, out string failReason) {
                if(instructions == null || pic < 0 || pic >= instructions.Count) {
                    instr = null;
                    failReason = "out of instructions";
                    return false;
                }

                if(abort) {
                    instr = null;
                    failReason = abortReason;
                    return false;
                }

                instr = instructions[pic++];

                // if maybe zero arg instruction
                if(instr.args.Count == 0) {
                    bool isOneOfThese = true;

                    switch(instr.name) {
                        case "cli":
                            interruptsEnabled = false;
                            break;
                        case "sei":
                            interruptsEnabled = true;
                            break;
                        default:
                            isOneOfThese = false;
                            break;
                    }

                    if(isOneOfThese) {
                        failReason = null;
                        return true;
                    }
                }

                // if maybe one arg instruction
                if(instr.args.Count == 1) {
                    bool isOneOfThese = true;

                    switch(instr.name) {
                        case "jmp":
                            Jump(instr.args[0].intValue);
                            break;
                        case "call":
                            Call(instr.args[0].intValue);
                            break;
                        case "int":
                            Interrupt(instr.args[0].stringValue);
                            break;
                        case "ret":
                            if(!Ret()) {
                                failReason = "no address to return from";
                                return false;
                            }

                            break;
                        case "push":
                            if(stack.Count == maxStackSize) {
                                failReason = $"maximum stack size exceeded ({maxStackSize})";
                                return false;
                            }

                            stack.Add(instr.args[0].floatValue);
                            break;
                        default:
                            isOneOfThese = false;
                            break;
                    }

                    if(isOneOfThese) {
                        failReason = null;
                        return true;
                    }
                }

                // if maybe instruction that assigns to variable as first arg
                if(instr.args.Count > 0 && instr.args[0].paramType == Compiler.ParamType.NumVariable) {
                    bool isOneOfThese = true;
                    float result = 0;

                    switch(instr.name) {
                        case "pop":
                            if(stack.Count == 0) {
                                failReason = "stack empty";
                                return false;
                            }

                            result = stack[stack.Count - 1];
                            stack.RemoveAt(stack.Count - 1);
                            break;
                        case "mov":
                            result = instr.args[1].floatValue;
                            break;
                        default:
                            isOneOfThese = false;
                            break;
                    }

                    if(isOneOfThese) {
                        instr.args[0].floatValue = result;
                        instr.args[0].intValue = (int)result;
                        failReason = null;
                        return true;
                    }
                }

                // if might be one arg math instruction
                if(instr.args.Count == 1 && instr.args[0].paramType == Compiler.ParamType.Num) {
                    var valueArg = instr.args[0];
                    float result = 0;
                    bool isOneOfThese = true;
                    
                    try {
                        switch(instr.name) {
                            case "abs":
                                result = Mathf.Abs(valueArg.floatValue);
                                break;
                            case "sign":
                                result = Mathf.Sign(valueArg.floatValue);
                                break;
                            case "sqrt":
                                result = Mathf.Sqrt(valueArg.floatValue);
                                break;
                            case "round":
                                result = Mathf.Round(valueArg.floatValue);
                                break;
                            case "floor":
                                result = Mathf.Floor(valueArg.floatValue);
                                break;
                            case "ceil":
                                result = Mathf.Ceil(valueArg.floatValue);
                                break;
                            default:
                                isOneOfThese = false;
                                break;
                        }

                        if(isOneOfThese) {
                            if(!float.IsFinite(result)) {
                                failReason = $"math error: {instr.name} {String.Join(" ", instr.args.Select(x => x.rawValue))} => {result}";
                                return false;
                            }

                            WriteVariable("rslt", result);
                            failReason = null;
                            return true;
                        }
                    } catch(ArithmeticException e) {
                        failReason = e.ToString();
                        return false;
                    }
                }

                // if might be two arg math instruction
                if(instr.args.Count == 2 && instr.args[0].paramType == Compiler.ParamType.Num && instr.args[1].paramType == Compiler.ParamType.Num) {
                    var lhsArg = instr.args[0];
                    var rhsArg = instr.args[1];
                    float result = 0;
                    bool isOneOfThese = true;
                    
                    try {
                        switch(instr.name) {
                            case "add":
                                result = lhsArg.floatValue + rhsArg.floatValue;
                                break;
                            case "sub":
                                result = lhsArg.floatValue - rhsArg.floatValue;
                                break;
                            case "mul":
                                result = lhsArg.floatValue * rhsArg.floatValue;
                                break;
                            case "div":
                                result = lhsArg.floatValue / rhsArg.floatValue;
                                break;
                            case "pow":
                                result = Mathf.Pow(lhsArg.floatValue, rhsArg.floatValue);
                                break;
                            case "min":
                                result = Mathf.Min(lhsArg.floatValue, rhsArg.floatValue);
                                break;
                            case "max":
                                result = Mathf.Max(lhsArg.floatValue, rhsArg.floatValue);
                                break;
                            default:
                                isOneOfThese = false;
                                break;
                        }

                        if(isOneOfThese) {
                            if(!float.IsFinite(result)) {
                                failReason = $"math error: {instr.name} {String.Join(" ", instr.args.Select(x => x.rawValue))} => {result}";
                                return false;
                            }

                            WriteVariable("rslt", result);
                            failReason = null;
                            return true;
                        }
                    } catch(ArithmeticException e) {
                        failReason = e.ToString();
                        return false;
                    }
                }

                if(instr.name == "lerp") {
                    float result = Mathf.Lerp(instr.args[0].floatValue, instr.args[1].floatValue, instr.args[2].floatValue);
                    WriteVariable("rslt", result);
                    failReason = null;
                    return true;
                }

                // if might be conditional jump instruction
                if(instr.args.Count == 3 && instr.args[0].paramType == Compiler.ParamType.Num && instr.args[1].paramType == Compiler.ParamType.Num && instr.args[2].paramType == Compiler.ParamType.Address) {
                    float lhs = instr.args[0].floatValue;
                    float rhs = instr.args[1].floatValue;
                    int addr = instr.args[2].intValue;
                    bool jump = false;

                    switch(instr.name) {
                        case "ja":
                            jump = Mathf.Approximately(lhs, rhs);
                            break;
                        case "je":
                            jump = lhs == rhs;
                            break;
                        case "jne":
                            jump = lhs != rhs;
                            break;
                        case "jna":
                            jump = !Mathf.Approximately(lhs, rhs);
                            break;
                        case "jg":
                            jump = lhs > rhs;
                            break;
                        case "jge":
                            jump = lhs >= rhs;
                            break;
                        case "jl":
                            jump = lhs < rhs;
                            break;
                        case "jle":
                            jump = lhs <= rhs;
                            break;
                    }

                    if(jump) {
                        Jump(addr);
                        failReason = null;
                        return true;
                    }
                }

                failReason = null;
                return true;
            }
        }

        class Compiler {
            List<string[]> tokens = new List<string[]>();
            public List<string> errors = new List<string>();
            public List<Instruction> instructions = new List<Instruction>();

            public enum ParamType {
                Num,
                Identifier,
                Address,
                NumVariable
            }

            public class Instruction {
                public class Argument {
                    public string name;
                    public ParamType paramType;
                    public ParamType argType;
                    public string rawValue;
                    public string stringValue;
                    public Argument variableReference = null;

                    private int _intValue;
                    public int intValue {
                        get {
                            if(variableReference == null) {
                                return _intValue;
                            } else {
                                return variableReference._intValue;
                            }
                        } set {
                            if(variableReference == null) {
                                _intValue = value;
                            } else {
                                variableReference._intValue = value;
                            }
                        }
                    }

                    private float _floatValue;
                    public float floatValue {
                        get {
                            if(variableReference == null) {
                                return _floatValue;
                            } else {
                                return variableReference._floatValue;
                            }
                        } set {
                            if(variableReference == null) {
                                _floatValue = value;
                            } else {
                                variableReference._floatValue = value;
                            }
                        }
                    }
                }

                public string name;
                public List<Argument> args;

                public Instruction(string name) {
                    this.name = name;
                    this.args = new List<Argument>();
                }

                public override string ToString() {
                    return $"{name} {String.Join(" ", args.Select(x => $"<{x.name}:{x.paramType}>"))}";
                }
            }

            struct Param {
                public ParamType type;
                public string name;

                public Param(string name, ParamType type) {
                    this.name = name;
                    this.type = type;
                }
            }

            Dictionary<string, Param[]> instructionDefs = new Dictionary<string, Param[]> {
                {"label", new Param[] { new Param("name", ParamType.Identifier) }},
                {"isr", new Param[] { new Param("name", ParamType.Identifier) }},
                {"jmp", new Param[] { new Param("label_name", ParamType.Address) }},
                {"call", new Param[] { new Param("label_name", ParamType.Address) }},
                {"int", new Param[] { new Param("isr_name", ParamType.Identifier) }},
                {"ret", new Param[] {  }},
                {"nop", new Param[] {  }},
                {"cli", new Param[] {  }},
                {"sei", new Param[] {  }},
                {"num", new Param[] { new Param("var_name_decl", ParamType.Identifier) }},
                {"push", new Param[] { new Param("value", ParamType.Num) }},

                {"pop", new Param[] { new Param("var_name", ParamType.NumVariable) }},
                {"mov", new Param[] { new Param("dest/lhs", ParamType.NumVariable), new Param("rhs", ParamType.Num) }},

                {"lerp", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("t", ParamType.Num) }},

                {"add", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num) }},
                {"sub", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num) }},
                {"mul", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num) }},
                {"div", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num) }},
                {"pow", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num) }},
                {"min", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num) }},
                {"max", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num) }},

                {"abs", new Param[] { new Param("value", ParamType.Num) }},
                {"sign", new Param[] { new Param("value", ParamType.Num) }},
                {"sqrt", new Param[] { new Param("value", ParamType.Num) }},
                {"floor", new Param[] { new Param("value", ParamType.Num) }},
                {"ceil", new Param[] { new Param("value", ParamType.Num) }},
                {"round", new Param[] { new Param("value", ParamType.Num) }},

                {"ja", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("label_name", ParamType.Address) }},
                {"je", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("label_name", ParamType.Address) }},
                {"jne", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("label_name", ParamType.Address) }},
                {"jna", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("label_name", ParamType.Address) }},
                {"jg", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("label_name", ParamType.Address) }},
                {"jge", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("label_name", ParamType.Address) }},
                {"jl", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("label_name", ParamType.Address) }},
                {"jle", new Param[] { new Param("lhs", ParamType.Num), new Param("rhs", ParamType.Num), new Param("label_name", ParamType.Address) }},

                {"getin", new Param[] { new Param("dest", ParamType.NumVariable), new Param("index", ParamType.Num) }},
                {"getout", new Param[] { new Param("dest", ParamType.NumVariable), new Param("index", ParamType.Num) }},
                {"setout", new Param[] { new Param("index", ParamType.Num), new Param("value", ParamType.Num) }},
                {"gettime", new Param[] { new Param("dest", ParamType.NumVariable)}},
                {"print", new Param[] { new Param("arg1", ParamType.NumVariable), new Param("arg2", ParamType.Num) }}
            };

            public bool Compile(string code) {
                errors.Clear();
                code = Regex.Replace(code, @"#.*$", "", RegexOptions.Multiline);

                Tokenize(code);

                // registers r0 - r7
                for(int i = 0; i < 8; i++) {
                    tokens.Add(new string[]{"num", "r" + i});
                }

                // rslt register
                tokens.Add(new string[]{"num", "rslt"});

                if(!Parse()) {
                    return false;
                }

                var labelAddresses = new Dictionary<string, int>();
                var variables = new Dictionary<string, Instruction.Argument>();

                for(int i = 0; i < instructions.Count; i++) {
                    var instr = instructions[i];

                    if(instr.name == "label") {
                        labelAddresses.Add(instr.args[0].stringValue, i);
                    }

                    if(instr.name == "num") {
                        var arg = instr.args[0];

                        if(variables.ContainsKey(arg.stringValue)) {
                            var message = plugin.lang.GetMessage("var_already_declared", plugin);
                            errors.Add(string.Format(message, i, arg.stringValue));
                            return false;
                        }

                        variables.Add(arg.stringValue, arg);
                    }
                }

                for(int i = 0; i < instructions.Count; i++) {
                    var instr = instructions[i];

                    for(int j = 0; j < instr.args.Count; j++) {
                        var arg = instr.args[j];

                        if(arg.paramType == ParamType.Address) {
                            int addr;
        
                            if(!labelAddresses.TryGetValue(arg.stringValue, out addr)) {
                                var message = plugin.lang.GetMessage("invalid_label", plugin);
                                errors.Add(string.Format(message, i, arg.stringValue));
                                return false;
                            }

                            arg.intValue = addr;
                        }

                        if(arg.argType == ParamType.NumVariable) {
                            Instruction.Argument variableArg;
                            if(!variables.TryGetValue(arg.stringValue, out variableArg)) {
                                var message = plugin.lang.GetMessage("var_not_declared", plugin);
                                errors.Add(string.Format(message, i, arg.stringValue));
                                return false;
                            } else {
                                arg.variableReference = variableArg;
                            }
                        }

                        if(arg.paramType == ParamType.Num) {
                            if(arg.argType != ParamType.Num && arg.argType != ParamType.NumVariable) {
                                var message = plugin.lang.GetMessage("incompat_arg_type", plugin);
                                errors.Add(string.Format(message, i, arg.stringValue, arg.argType, instr.ToString()));
                                return false;
                            }
                        }
                    }
                }

                return true;
            }

            bool ProcessInstruction(int line, string instr, string[] args, Param[] parameters) {
                var compiledInstruction = new Instruction(instr);

                Action<string> fail = (string arg) => {
                    var message = plugin.lang.GetMessage("invalid_arg", plugin);
                    errors.Add(string.Format(message, line, arg, instr, String.Join(" ", parameters.Select(x => x.name + ':' + x.type))));
                };

                Action<int, float, int, string, ParamType> addArgument = (int index, float floatValue, int intValue, string stringValue, Compiler.ParamType argType) => {
                    compiledInstruction.args.Add(new Instruction.Argument {
                        name = parameters[index].name,
                        paramType = parameters[index].type,
                        argType = argType,
                        rawValue = args[index],
                        floatValue = floatValue,
                        intValue = intValue,
                        stringValue = stringValue
                    });
                };

                for(int i = 0; i < args.Length; i++) {
                    var arg = args[i];
                    var param = parameters[i];

                    if(param.type == ParamType.Num) {
                        var match = Regex.Match(arg, @"^[+-]*[0-9]*[\.]?[0-9]*$");
                        var matchMapGridCol = Regex.Match(arg, @"^([a-zA-Z]{1,2})[\.]([0-9]*)?$");
                        var matchNumVar = Regex.Match(arg, @"^[a-zA-Z_][a-zA-Z0-9_]*$");

                        if(!match.Success && !matchMapGridCol.Success && !matchNumVar.Success) {
                            fail(arg);
                            return false;
                        }

                        if(match.Success) {
                            if(match.Groups[0].ToString() == ".") {
                                fail(arg);
                                return false;
                            }

                            float value;
                            if(!float.TryParse(arg, out value)) {
                                fail(arg);
                                return false;
                            }

                            addArgument(i, value, (int)value, arg, param.type);
                        } else if(matchMapGridCol.Success) {
                            var lettersStr = matchMapGridCol.Groups[1].ToString().ToUpper();
                            var lettersFractionStr = matchMapGridCol.Groups[2].ToString();

                            int lettersWhole = (lettersStr.Length == 1) ? lettersStr[0] - 'A' : lettersStr[1] + 26 - 'A';
                            float lettersFraction = 0.0f;

                            if(lettersFractionStr != "." && lettersFractionStr.Length != 0) {
                                if(!float.TryParse(lettersFractionStr, out lettersFraction)) {
                                    fail(arg);
                                    return false;
                                }
                            }

                            var value = lettersWhole + lettersFraction;
                            
                            addArgument(i, value, (int)value, arg, param.type);
                        } else if(matchNumVar.Success) {
                            addArgument(i, 0, 0, arg, ParamType.NumVariable);
                        }
                    }

                    if(param.type == ParamType.Identifier || param.type == ParamType.Address || param.type == ParamType.NumVariable) {
                        var match = Regex.Match(arg, @"^[a-zA-Z_][a-zA-Z0-9_]*$");

                        if(!match.Success) {
                            fail(arg);
                            return false;
                        }
                        
                        addArgument(i, 0, 0, arg, param.type);
                    }
                }

                instructions.Add(compiledInstruction);
                return true;
            }

            bool Parse() {
                instructions.Clear();

                for(int i = 0; i < tokens.Count; i++) {
                    var line = tokens[i];
                    var instr = line[0];

                    Param[] parameters;
                    var found = instructionDefs.TryGetValue(instr, out parameters);

                    if(!found) {
                        var message = plugin.lang.GetMessage("invalid_instruction", plugin);
                        errors.Add(string.Format(message, i, instr));
                        return false;
                    }

                    if(parameters.Length != line.Length - 1) {
                        var message = plugin.lang.GetMessage("wrong_num_args", plugin);
                        errors.Add(string.Format(message, i, instr, String.Join(" ", parameters.Select(x => x.name + ':' + x.type))));
                        return false;
                    }

                    if(!ProcessInstruction(i, instr, line.Skip(1).ToArray(), parameters)) {
                        return false;
                    }
                }

                return true;
            }

            void Tokenize(string code) {
                tokens.Clear();
                var lines = code.Split(new[] {"\r\n", "\r", "\n", ";"}, StringSplitOptions.RemoveEmptyEntries);

                for(int i = 0; i < lines.Length; i++) {
                    lines[i] = code = Regex.Replace(lines[i].Trim() ,@"\s+"," ");

                    if(lines[i].Length > 0) {
                        if(lines[i][0] == '#') {
                            continue;
                        }

                        var args = lines[i].Split(' ');

                        if(args.Length > 0) {
                            tokens.Add(args);
                        }
                    }
                }
            }
        }
    }
}