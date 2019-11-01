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
        static VirtualCPUAssembler compiler = null;
        string shortName = "electrical.random.switch.deployed";
        string prefab = "assets/prefabs/deployable/playerioents/gates/randswitch/electrical.random.switch.deployed.prefab";
        static int numChannels = 4;
        bool ignoreSpawn = false;
        ulong McuId = 0;
        Dictionary<ulong, McuComponent> McuComponents = new Dictionary<ulong, McuComponent>();
        Dictionary<uint, IOEntityMapper> ioEnts = new Dictionary<uint, IOEntityMapper>();
        McuManager manager = null;
        const int maxAvgCharsPerLine = 32;

        class VirtualCPUAssembler {
            List<string[]> codeLines = new List<string[]>();
            List<Token[]> tokens = new List<Token[]>();
            Dictionary<string, Symbol> symbols = new Dictionary<string, Symbol>();
            public List<string> errors = new List<string>();
            public List<VirtualCPU.Word> programData = new List<VirtualCPU.Word>();
            public List<VirtualCPU.Instruction> instructions = new List<VirtualCPU.Instruction>();
            public VirtualCPU.Word[] memory = null;

            struct Symbol {
                public VirtualCPU.Word word;
                public Type type;

                public enum Type {
                    ADDRESS,
                    REGISTER
                }
            }

            struct Token {
                public enum Type {
                    NONE,
                    DIRECTIVE,
                    LABEL,
                    INSTRUCTION,
                    IDENTIFIER,
                    INTEGER,
                    FLOAT
                }

                public bool indirect;
                public Type type;
                public string stringValue;
                public int intValue;
                public float floatValue;
                public VirtualCPU.Word word;

                public override string ToString() {
                    return $"{{{type.ToString()}:{stringValue} indirect:{indirect} intValue:{intValue} floatValue:{floatValue}}}";
                }
            }

            public bool Compile(string code, int memorySize) {
                if(!Preprocess(code)) {
                    return false;
                }

                if(!Tokenize()) {
                    return false;
                }

                if(!Parse()) {
                    return false;
                }

                if(memorySize < programData.Count) {
                    errors.Add($"program data is larger than desired memory size ({memorySize} < {programData.Count})");
                    return false;
                }

                memory = new VirtualCPU.Word[memorySize];
                programData.CopyTo(memory, 0);

                return true;
            }

            bool TryStringToOpcode(string str, out VirtualCPU.Opcode opcode) {
                var names = Enum.GetNames(typeof(VirtualCPU.Opcode));
                var values = Enum.GetValues(typeof(VirtualCPU.Opcode));
                str = str.ToUpperInvariant();
                
                for(int i = 0; i < names.Length; i++) {
                    if(names[i] == str) {
                        opcode = (VirtualCPU.Opcode)values.GetValue(i);
                        return true;
                    }
                }

                opcode = 0;
                return false;
            }

            void AllocateRegisters() {
                var names = Enum.GetNames(typeof(VirtualCPU.Register));

                for(int i = 0; i < names.Length; i++) {
                    programData.Add(VirtualCPU.Word.Create(0));

                    symbols.Add(names[i].ToLowerInvariant(), new Symbol {
                        word = VirtualCPU.Word.Create(i),
                        type = Symbol.Type.REGISTER
                    });
                }
            }

            bool Parse() {
                AllocateRegisters();
                int numInstructions = 0;

                for(int i = 0; i < tokens.Count; i++) {
                    Token[] line = tokens[i];

                    if(line[0].type == Token.Type.LABEL) {
                        if(symbols.ContainsKey(line[0].stringValue)) {
                            errors.Add($"redefinition of identifier \"{line[0].stringValue}\"");
                            return false;
                        } else {
                            symbols.Add(line[0].stringValue, new Symbol {
                                word = VirtualCPU.Word.Create(numInstructions),
                                type = Symbol.Type.ADDRESS
                            });
                        }
                    } else if(line[0].type == Token.Type.INSTRUCTION) {
                        numInstructions += line.Length;
                    }
                }

                for(int i = 0; i < tokens.Count; i++) {
                    Token[] line = tokens[i];

                    if(line[0].type == Token.Type.DIRECTIVE) {
                        if(line[0].stringValue == "const" || line[0].stringValue == "word") {
                            if(line.Length != 3 || line[1].type != Token.Type.IDENTIFIER || (line[2].type != Token.Type.INTEGER && line[2].type != Token.Type.FLOAT)) {
                                errors.Add($"invalid directive");
                                return false;
                            }

                            if(symbols.ContainsKey(line[1].stringValue)) {
                                errors.Add($"redefinition of identifier \"{line[1].stringValue}\"");
                                return false;
                            } else {
                                if(line[0].stringValue == "const") {
                                    symbols.Add(line[1].stringValue, new Symbol {
                                        word = line[2].word,
                                        type = Symbol.Type.ADDRESS
                                    });
                                } else {
                                    programData.Add(line[2].word);

                                    symbols.Add(line[1].stringValue, new Symbol {
                                        word = VirtualCPU.Word.Create(programData.Count - 1),
                                        type = Symbol.Type.ADDRESS
                                    });
                                }
                            }
                        } else if(line[0].stringValue == "isr") {
                            if(line.Length != 3 || line[1].type != Token.Type.IDENTIFIER ||line[2].type != Token.Type.IDENTIFIER) {
                                errors.Add($"invalid directive");
                                return false;
                            }

                            Symbol labelAddr0;
                            Symbol labelAddr1;

                            if(!symbols.TryGetValue(line[1].stringValue, out labelAddr0)) {
                                errors.Add($"invalid isr directive, no identifier \"{line[1].stringValue}\"");
                                return false;
                            } else if(labelAddr0.type != Symbol.Type.ADDRESS) {
                                errors.Add($"invalid isr directive, identifier \"{line[1].stringValue}\" is not an address");
                                return false;
                            }

                            if(!symbols.TryGetValue(line[2].stringValue, out labelAddr1)) {
                                errors.Add($"invalid isr directive, no identifier \"{line[2].stringValue}\"");
                                return false;
                            } else if(labelAddr1.type != Symbol.Type.ADDRESS) {
                                errors.Add($"invalid isr directive, identifier \"{line[2].stringValue}\" is not an address");
                                return false;
                            }

                            var inst = instructions[labelAddr0.word.Int + 1];
                            inst.word.Int = labelAddr1.word.Int;
                            instructions[labelAddr0.word.Int + 1] = inst;
                        } else {
                            errors.Add($"unknown directive \"{line[0].stringValue}\"");
                            return false;
                        }
                    } else if(line[0].type == Token.Type.INSTRUCTION) {
                        VirtualCPU.Opcode opcode;

                        if(!TryStringToOpcode(line[0].stringValue, out opcode)) {
                            errors.Add($"unknown opcode \"{line[0].stringValue}\"");
                            return false;
                        } else if(line.Length > 3) {
                            errors.Add($"no instructions take more than two arguments");
                            return false;
                        }

                        instructions.Add(VirtualCPU.Instruction.Create(opcode));
                        int instIndex = instructions.Count - 1;

                        for(int j = 1; j < line.Length; j++) {
                            int argNum = j - 1;

                            if(line[j].type == Token.Type.IDENTIFIER) {
                                Symbol symbol;

                                if(!symbols.TryGetValue(line[j].stringValue, out symbol)) {
                                    errors.Add($"unknown identifier \"{line[j].stringValue}\"");
                                    return false;
                                }

                                if(symbol.type == Symbol.Type.REGISTER) {
                                    instructions[instIndex] = instructions[instIndex].SetArgIsRegister(argNum, true);
                                }

                                instructions.Add(VirtualCPU.Instruction.Create(symbol.word));
                            } else if(line[j].type == Token.Type.INTEGER || line[j].type == Token.Type.FLOAT) {
                                instructions.Add(VirtualCPU.Instruction.Create(line[j].word));
                            } else {
                                errors.Add($"invalid instruction argument \"{line[j].stringValue}\"");
                                return false;
                            }

                            instructions[instIndex] = instructions[instIndex].SetArgIsIndirect(argNum, line[j].indirect);
                        }

                        instructions[instIndex] = instructions[instIndex].SetNumArgs(line.Length - 1);
                    }
                }

                return true;
            }

            bool Tokenize() {
                for(int i = 0; i < codeLines.Count; i++) {
                    var line = codeLines[i];
                    tokens.Add(new Token[line.Length]);
                    var tokenLine = tokens[i];

                    for(int j = 0; j < line.Length; j++) {
                        string arg = line[j];
                        tokenLine[j].indirect = false;
                        tokenLine[j].type = Token.Type.NONE;
                        Match indirectMatch = Regex.Match(arg, @"^\[([^\[]+)\]$");
                        
                        if(indirectMatch.Success) {
                            tokenLine[j].indirect = true;
                            arg = indirectMatch.Groups[1].ToString();
                        }

                        Match directiveMatch = Regex.Match(arg, @"^\.([a-zA-Z_][a-zA-Z0-9_]*)$");
                        Match labelMatch = Regex.Match(arg, @"^([a-zA-Z_][a-zA-Z0-9_]*):$");
                        Match identifierMatch = Regex.Match(arg, @"^[a-zA-Z_][a-zA-Z0-9_]*$");
                        Match integerMatch = Regex.Match(arg, @"^[+-]?[0-9]+$");
                        Match floatMatch = Regex.Match(arg, @"^[+-]?[0-9]?[\.][0-9]*$");

                        if(directiveMatch.Success) {
                            tokenLine[j].type = Token.Type.DIRECTIVE;
                            tokenLine[j].stringValue = directiveMatch.Groups[1].ToString();
                        } else if(labelMatch.Success) {
                            tokenLine[j].type = Token.Type.LABEL;
                            tokenLine[j].stringValue = labelMatch.Groups[1].ToString();
                        } else if(identifierMatch.Success) {
                            tokenLine[j].type = (j == 0) ? Token.Type.INSTRUCTION : Token.Type.IDENTIFIER;
                            tokenLine[j].stringValue = identifierMatch.Groups[0].ToString();
                        } else if(integerMatch.Success) {
                            tokenLine[j].type = Token.Type.INTEGER;
                            tokenLine[j].stringValue = integerMatch.Groups[0].ToString();
                            int.TryParse(tokenLine[j].stringValue, out tokenLine[j].word.Int);
                        } else if(floatMatch.Success && floatMatch.Groups[0].ToString() != ".") {
                            tokenLine[j].type = Token.Type.FLOAT;
                            tokenLine[j].stringValue = floatMatch.Groups[0].ToString();
                            float.TryParse(tokenLine[j].stringValue, out tokenLine[j].word.Float);
                        }
                    }
                }

                return true;
            }

            bool Preprocess(string code) {
                var lines = code.Split(new[] {"\r\n", "\r", "\n", ";"}, StringSplitOptions.RemoveEmptyEntries);
                
                for(int i = 0; i < lines.Length; i++) {
                    lines[i] = code = Regex.Replace(lines[i].Trim() ,@"\s+"," ");

                    if(lines[i].Length > 0) {
                        if(lines[i][0] == '#') {
                            continue;
                        }

                        var args = lines[i].Split(' ');

                        if(args.Length > 0) {
                            codeLines.Add(args);
                        }
                    }
                }

                return true;
            }
        }

        class VirtualCPU {
            Word[] memory = null;
            Instruction[] instructions = null;
            Word cmp0 = Word.Create(0);
            Word cmp1 = Word.Create(0);
            Word flags = Word.Create((uint)Flags.INTERRUPTS_ENABLED);
            Queue<int> pendingInterrupts = new Queue<int>();
            static int maxPendingInterrupts = 8;
            IPeripheral peripheral = null;
            System.Random random = new System.Random();

            public VirtualCPU(IPeripheral perip) {
                peripheral = perip;
            }

            int pic = 0;
            int _sp = 0;
            int sp {
                get { return _sp; }
                set { _sp = value; memory[(int)Register.SP].Int = value; }
            }

            public void Reset() {
                memory = null;
                instructions = null;
                cmp0 = Word.Create(0);
                cmp1 = Word.Create(0);
                flags = Word.Create((uint)Flags.INTERRUPTS_ENABLED);
                pendingInterrupts.Clear();
            }

            public abstract class IPeripheral {
                public abstract Word Out(Word addr);
                public abstract void In(Word addr, Word value);
            }

            [Flags]
            enum Flags : uint {
                INTERRUPTS_ENABLED = 1 << 0
            }

            [StructLayout(LayoutKind.Explicit)]
            public struct Word {
                [FieldOffset(0)]
                public int Int;
                [FieldOffset(0)]
                public uint Uint;
                [FieldOffset(0)]
                public float Float;

                public static Word Create(int i) {
                    return new Word{ Int = i };
                }

                public static Word Create(uint u) {
                    return new Word{ Uint = u };
                }

                public static Word Create(float f) {
                    return new Word{ Float = f };
                }
            }

            [StructLayout(LayoutKind.Explicit)]
            public struct Instruction {
                [FieldOffset(0)]
                public Word word;
                [FieldOffset(0)]
                public Opcode op;
                [FieldOffset(2)]
                public ushort argFlags;

                public Instruction SetNumArgs(int num) {
                    argFlags = (ushort)((argFlags & ~0xF) | num);
                    return this;
                }

                public Instruction SetArgIsRegister(int argNum, bool value) {
                    int shift = 4 + argNum;
                    argFlags = (value) ? (ushort)(argFlags | 1 << shift) : (ushort)(argFlags & ~(1 << shift));
                    return this;
                }

                public Instruction SetArgIsIndirect(int argNum, bool value) {
                    int shift = 8 + argNum;
                    argFlags = (value) ? (ushort)(argFlags | 1 << shift) : (ushort)(argFlags & ~(1 << shift));
                    return this;
                }

                public static Instruction Create(Word word) {
                    return new Instruction{ word = word };
                }

                public static Instruction Create(int i) {
                    return new Instruction{ word = new Word{ Int = i } };
                }

                public static Instruction Create(Opcode op) {
                    return new Instruction{ op = op, argFlags = 0 };
                }
            }

            public enum Register {
                R0, R1, R2,  R3,  R4,  R5,  R6,  R7,
                R8, R9, R10, R11, R12, R13, R14, R15,
                SP
            }

            public enum Opcode : byte {
                NOP = 0,
                MOV,
                JMP,
                CALL,
                RET,
                PUSH,
                POP,
                SHRS,
                SHRU,
                SHL,
                CMPI,
                JE,
                JNE,
                JG,
                JGE,
                JL,
                JLE,
                AND,
                OR,
                XOR,
                NOT,
                ADD,
                SUB,
                MUL,
                DIV,
                MOD,
                IN,
                OUT,
                CLI,
                SEI,
                RNGI
            }

            public enum Status {
                SUCCESS,
                OUT_OF_INSTRUCTIONS,
                MISSING_INSTRUCTION,
                BAD_INSTRUCTION,
                SEGFAULT,
                DIVISION_BY_ZERO
            }

            public void LoadProgram(Instruction[] instructions, Word[] memory, int bsp, int pic = 0) {
                Reset();

                this.instructions = new Instruction[instructions.Length];
                instructions.CopyTo(this.instructions, 0);
                this.memory = new Word[memory.Length];
                memory.CopyTo(this.memory, 0);
                this.pic = pic;
                this.sp = bsp;
            }

            public bool Interrupt(int addr) {
                if(pendingInterrupts.Count >= maxPendingInterrupts) {
                    return false;
                }

                pendingInterrupts.Enqueue(addr);
                return true;
            }

            public bool Cycle(out Status status) {
                if(pic < 0 || pic >= instructions.Length) {
                    status = Status.OUT_OF_INSTRUCTIONS;
                    return false;
                }

                if(sp < 0 || sp >= memory.Length) {
                    status = Status.SEGFAULT;
                    return false;
                }

                if((flags.Int & (int)Flags.INTERRUPTS_ENABLED) != 0 && pendingInterrupts.Count != 0) {
                    int addr = pendingInterrupts.Dequeue();

                    memory[sp++].Int = pic;
                    pic = addr;

                    if(pic < 0 || pic >= instructions.Length) {
                        status = Status.OUT_OF_INSTRUCTIONS;
                        return false;
                    }

                    if(sp < 0 || sp >= memory.Length) {
                        status = Status.SEGFAULT;
                        return false;
                    }
                }

                Instruction inst = instructions[pic++];
                int numArgs = inst.argFlags & 0xF;
                int argRegFlags = (inst.argFlags & 0xF0) >> 4;
                int argIndirectFlags = (inst.argFlags & 0xF00) >> 8;
                Word arg0 = Word.Create(0), arg1 = arg0;
                Word addr0 = Word.Create(0), addr1 = addr0;
                bool handledHere;

                if(numArgs >= 1) {
                    if(pic < 0 || pic >= instructions.Length) {
                        status = Status.OUT_OF_INSTRUCTIONS;
                        return false;
                    }

                    arg0 = instructions[pic++].word;
                }

                if(numArgs >= 2) {
                    if(pic < 0 || pic >= instructions.Length) {
                        status = Status.OUT_OF_INSTRUCTIONS;
                        return false;
                    }

                    arg1 = instructions[pic++].word;
                }

                if((argIndirectFlags & 1) != 0) {
                    if(arg0.Int < 0 || arg0.Int >= memory.Length) {
                        status = Status.SEGFAULT;
                        return false;
                    }

                    arg0 = memory[arg0.Int];
                }

                if((argIndirectFlags & 2) != 0) {
                    if(arg1.Int < 0 || arg1.Int >= memory.Length) {
                        status = Status.SEGFAULT;
                        return false;
                    }

                    arg1 = memory[arg1.Int];
                }

                addr0 = arg0;
                addr1 = arg1;

                if((argRegFlags & 1) != 0) {
                    if(arg0.Int < 0 || arg0.Int >= memory.Length) {
                        status = Status.SEGFAULT;
                        return false;
                    }

                    arg0 = memory[arg0.Int];
                }

                if((argRegFlags & 2) != 0) {
                    if(arg1.Int < 0 || arg1.Int >= memory.Length) {
                        status = Status.SEGFAULT;
                        return false;
                    }

                    arg1 = memory[arg1.Int];
                }

                handledHere = true;

                // these require no additional checks
                switch(inst.op) {
                    case Opcode.JMP:
                        pic = arg0.Int;
                        break;
                    case Opcode.CALL:
                        memory[sp++].Int = pic;
                        pic = arg0.Int;
                        break;
                    case Opcode.RET:
                        pic = memory[sp--].Int;
                        break;
                    case Opcode.PUSH:
                        memory[sp++] = arg0;
                        break;
                    case Opcode.CMPI:
                        cmp0.Int = arg0.Int;
                        cmp1.Int = arg1.Int;
                        break;
                    case Opcode.JE:
                        pic = (cmp0.Int == cmp1.Int) ? arg0.Int : pic;
                        break;
                    case Opcode.JNE:
                        pic = (cmp0.Int != cmp1.Int) ? arg0.Int : pic;
                        break;
                    case Opcode.JG:
                        pic = (cmp0.Int > cmp1.Int) ? arg0.Int : pic;
                        break;
                    case Opcode.JGE:
                        pic = (cmp0.Int >= cmp1.Int) ? arg0.Int : pic;
                        break;
                    case Opcode.JL:
                        pic = (cmp0.Int < cmp1.Int) ? arg0.Int : pic;
                        break;
                    case Opcode.JLE:
                        pic = (cmp0.Int <= cmp1.Int) ? arg0.Int : pic;
                        break;
                    case Opcode.CLI:
                        flags.Uint &= ~(uint)Flags.INTERRUPTS_ENABLED;
                        break;
                    case Opcode.SEI:
                        flags.Uint |= (uint)Flags.INTERRUPTS_ENABLED;
                        break;
                    case Opcode.OUT:
                        if(peripheral != null) {
                            peripheral.In(arg0, arg1);
                        }

                        break;
                    default:
                        handledHere = false;
                        break;
                }

                if(handledHere) {
                    status = Status.SUCCESS;
                    return true;
                }

                // these all reference memory and need bounds check
                if(addr0.Int < 0 || addr0.Int >= memory.Length) {
                    status = Status.SEGFAULT;
                    return false;
                }

                handledHere = true;

                switch(inst.op) {
                    case Opcode.MOV:
                        memory[addr0.Int] = arg1;
                        break;
                    case Opcode.POP:
                        memory[addr0.Int] = memory[sp--];
                        break;
                    case Opcode.SHRS:
                        memory[addr0.Int].Int = arg0.Int >> arg1.Int;
                        break;
                    case Opcode.SHRU:
                        memory[addr0.Int].Uint = arg0.Uint >> arg1.Int;
                        break;
                    case Opcode.SHL:
                        memory[addr0.Int].Int = arg0.Int << arg1.Int;
                        break;
                    case Opcode.AND:
                        memory[addr0.Int].Int = arg0.Int & arg1.Int;
                        break;
                    case Opcode.OR:
                        memory[addr0.Int].Int = arg0.Int | arg1.Int;
                        break;
                    case Opcode.XOR:
                        memory[addr0.Int].Int = arg0.Int ^ arg1.Int;
                        break;
                    case Opcode.NOT:
                        memory[addr0.Int].Int = ~arg0.Int;
                        break;
                    case Opcode.ADD:
                        memory[addr0.Int].Int = arg0.Int + arg1.Int;
                        break;
                    case Opcode.SUB:
                        memory[addr0.Int].Int = arg0.Int - arg1.Int;
                        break;
                    case Opcode.MUL:
                        memory[addr0.Int].Int = arg0.Int * arg1.Int;
                        break;
                    case Opcode.DIV:
                        if(arg1.Int == 0) {
                            status = Status.DIVISION_BY_ZERO;
                            return false;
                        }

                        memory[addr0.Int].Int = arg0.Int / arg1.Int;
                        break;
                    case Opcode.MOD:
                        if(arg1.Int == 0) {
                            status = Status.DIVISION_BY_ZERO;
                            return false;
                        }

                        memory[addr0.Int].Int = arg0.Int % arg1.Int;
                        break;
                    case Opcode.IN:
                        if(peripheral != null) {
                            memory[addr0.Int] = peripheral.Out(arg1);
                        }
                        break;
                    case Opcode.RNGI:
                        memory[addr0.Int] = Word.Create(random.Next());
                        break;
                    default:
                        handledHere = false;
                        break;
                }

                if(handledHere) {
                    status = Status.SUCCESS;
                    return true;
                }

                status = Status.MISSING_INSTRUCTION;
                return false;
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
            compiler = new VirtualCPUAssembler();

            //Puts($"new GUID: {Guid.NewGuid()}");
        }

        void OnServerInitialized() {
            var go = new GameObject(McuManager.Guid);
            manager = go.AddComponent<McuManager>();
            manager.Init(this);
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

            McuComponent comp = new McuComponent();

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

            bool success = compiler.Compile(comp.setupCode + @"
            main:
                out channel 0
                out output_energy 2
                out channel 1
                out output_energy 2
                out channel 2
                out output_energy 2
                out channel 3
                out output_energy 2

                rngi r0
                out output_mask r0
                
                jmp main
            ", 1024);

            if(success) {
                comp.cpu.LoadProgram(compiler.instructions.ToArray(), compiler.memory, compiler.programData.Count);
            } else {
                foreach(var error in compiler.errors) {
                    Puts(error);
                }
            }

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
            static ulong idCtr = 0;
            public ulong id;
            public IOEntity[] channels = new IOEntity[numChannels];
            public int[] inputEnergies = new int[numChannels];
            public int[] outputEnergies = new int[numChannels];
            public VirtualCPU cpu = null;
            public BaseEntity stash = null;
            public string setupCode = @"
            .const channel 0
            .const output_energy 1
            .const input_energy 2
            .const output_mask 3

            jmp user_code

            input:
                jmp input_stub

            input_stub:
                ret

            user_code:
            ";

            public McuComponent() {
                cpu = new VirtualCPU(new Peripheral(this));
                id = idCtr++;
            }

            class Peripheral : VirtualCPU.IPeripheral {
                McuComponent comp = null;
                int[] desiredOutputEnergies = null;
                int[] maskedOutputEnergies = null;
                int selectedChannel = 0;
                int outputMask = 0;

                enum Port {
                    CHANNEL,
                    OUTPUT_ENERGY,
                    INPUT_ENERGY,
                    OUTPUT_MASK
                }
                
                public Peripheral(McuComponent comp) {
                    this.comp = comp;
                    desiredOutputEnergies = new int[comp.outputEnergies.Length];
                    maskedOutputEnergies = new int[comp.outputEnergies.Length];
                }

                void UpdateOutputEnergies() {
                    //Print("UpdateOutputEnergies()");

                    for(int i = 0; i < desiredOutputEnergies.Length; i++) {
                        maskedOutputEnergies[i] = ((outputMask & (1 << i)) != 0) ? desiredOutputEnergies[i] : 0;
                        //Print($"    outputMask: {Convert.ToString(outputMask, 2).PadLeft(32, '0')}");
                        //Print($"    desiredOutputEnergies[{i}]: {desiredOutputEnergies[i]}");
                        //Print($"    maskedOutputEnergies[{i}]: {maskedOutputEnergies[i]}");
                    }

                    comp.UpdateOutputEnergies(maskedOutputEnergies);
                }

                public override VirtualCPU.Word Out(VirtualCPU.Word addr) {
                    //Print($"Peripheral.Out({addr.Int})");

                    switch((Port)addr.Int) {
                        case Port.CHANNEL:
                            return VirtualCPU.Word.Create(selectedChannel);
                        case Port.OUTPUT_ENERGY:
                            return VirtualCPU.Word.Create(comp.outputEnergies[selectedChannel]);
                        case Port.INPUT_ENERGY:
                            return VirtualCPU.Word.Create(comp.inputEnergies[selectedChannel]);
                        case Port.OUTPUT_MASK:
                            return VirtualCPU.Word.Create(outputMask);
                    }

                    return VirtualCPU.Word.Create(0);
                }

                public override void In(VirtualCPU.Word addr, VirtualCPU.Word value) {
                    //Print($"Peripheral.In({addr.Int}, {value.Int})");

                    switch((Port)addr.Int) {
                        case Port.CHANNEL:
                            selectedChannel = Math.Max(0, Math.Min(value.Int, desiredOutputEnergies.Length - 1));
                            break;
                        case Port.OUTPUT_ENERGY:
                            desiredOutputEnergies[selectedChannel] = value.Int;
                            UpdateOutputEnergies();
                            break;
                        case Port.OUTPUT_MASK:
                            outputMask = value.Int;
                            UpdateOutputEnergies();
                            break;
                    }
                }
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
                //Print($"OnInputUpdate({inputAmount}, {inputSlot}) slot #{inputSlot} ioSlot.niceName: {ioSlot.niceName}");

                if(inputSlot != 0) {
                    return false;
                }

                if(inputAmount != inputEnergies[index]) {
                    inputEnergies[index] = inputAmount;
                    ioEnt.IOStateChanged(inputAmount, inputSlot);
                    //cpu.Interrupt(1);
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

                //Print($"UpdateSingleOutput({ioEntindex}, {outputIndex}, {energy}) ioSlot.niceName: {ioSlot.niceName}");

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
                                    bool success = compiler.Compile(setupCode + item.text, 1024);

                                    if(!success) { 
                                        item.text += "\n" + String.Join("\n", compiler.errors.Select(x => {
                                            return string.Format(plugin.lang.GetMessage("compiler_error", plugin), x);
                                        }));
                                    } else {
                                        Print("loaded instructions");
                                        cpu.LoadProgram(compiler.instructions.ToArray(), compiler.memory, compiler.programData.Count);
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
                cpu.Reset();
            }

            public void CycleCPU() {
                if(TotalInputEnergy() == 0) {
                    return;
                }

                int numInstructionsExecuted = 0;
                VirtualCPU.Status status;

                while(numInstructionsExecuted < config.maxInstructionsPerCycle) {
                    if(!cpu.Cycle(out status)) {
                        Print(status.ToString());
                        Reset();
                        return;
                    }

                    numInstructionsExecuted++;
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
    }
}