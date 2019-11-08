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
        static Assembler assembler = null;
        string shortName = "electrical.random.switch.deployed";
        string prefab = "assets/prefabs/deployable/playerioents/gates/randswitch/electrical.random.switch.deployed.prefab";
        static int numChannels = 4;
        bool ignoreSpawn = false;
        ulong McuId = 0;
        Dictionary<ulong, McuComponent> McuComponents = new Dictionary<ulong, McuComponent>();
        Dictionary<uint, IOEntityMapper> ioEnts = new Dictionary<uint, IOEntityMapper>();
        McuManager manager = null;
        const int maxAvgCharsPerLine = 32;

        class Assembler {
            List<Statement> statements = new List<Statement>();
            public Dictionary<string, Symbol> symbols = new Dictionary<string, Symbol>();
            public List<string> errors = new List<string>();
            public List<byte> programData = new List<byte>();
            public List<Instruction> instructions = new List<Instruction>();
            public List<uint> code = new List<uint>();
            public byte[] memory = null;
            public int numInstructions;
            List<KeyValuePair<Symbol, Symbol>> isrs = new List<KeyValuePair<Symbol, Symbol>>();

            static void Print(string msg) {
                Console.WriteLine(msg);
            }

            static void PrintVar<T>(string name, T var) {
                Print($"{name}: {var}");
            }

            public void Reset() {
                statements.Clear();
                symbols.Clear();
                errors.Clear();
                programData.Clear();
                instructions.Clear();
                code.Clear();
                memory = null;
                isrs.Clear();
            }

            struct Statement {
                public int lineNum;
                public string[] line;
                public Token[] tokens;
            }

            public struct Variable {
                public CPU.Value32 val32;
                public Type type;
                public int size;

                public enum Type {
                    NONE, UNKNOWN, INT, UINT, BYTE, FLOAT
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

                public int offset;
                public Type type;
                public string stringValue;
                public Variable var;
                public string cond;
            }

            public struct Symbol {
                public string name;
                public Variable var;
                public int labelInstructionIndex;
                public Type type;

                public enum Type {
                    NONE,
                    LABEL,
                    LITERAL,
                    CONSTANT,
                    REGISTER
                }
            }

            public void LoadProgramToCPU(CPU cpu) {
                cpu.Reset();
                cpu.instructions = new uint[numInstructions];
                cpu.memory = new byte[memory.Length];
                memory.CopyTo(cpu.memory, 0);
                cpu.registers[(int)CPU.Register.SP] = (uint)programData.Count;
                cpu.pc = symbols["main"].var.val32.Uint;
                int instructionIndex = 0;

                for(int i = 0; i < instructions.Count; i++) {
                    var instruction = instructions[i];
                    cpu.instructions[instructionIndex++] = instruction.Create();

                    if(instruction.additionalInstructions == null) {
                        continue;
                    }

                    for(int j = 0; j < instruction.additionalInstructions.Length; j++) {
                        cpu.instructions[instructionIndex++] = instruction.additionalInstructions[j];
                    }
                }

                cpu.flags |= (uint)CPU.Flag.READY;
            }

            public struct Instruction {
                public CPU.Opcode opcode;
                public CPU.Cond cond;
                public List<CPU.Register> operands;
                public Symbol immediate;
                public int address;
                public uint[] additionalInstructions;

                public int AdditionalInstructions(bool ignoreLabelAddresses) {
                    if(immediate.type == Symbol.Type.NONE || (immediate.type == Symbol.Type.LABEL && ignoreLabelAddresses)) {
                        return 0;
                    }

                    if(immediate.var.type == Variable.Type.FLOAT) {
                        return 1;
                    } else if(immediate.var.type == Variable.Type.UNKNOWN) {
                        return immediate.var.size / sizeof(uint);
                    } else if(immediate.var.type == Variable.Type.INT) {
                        if(immediate.var.val32.Uint >= GetMaxImmediateValue(operands.Count + 1)) {
                            return 1;
                        }
                    }

                    return 0;
                }

                public uint Create() {
                    uint instruction = (uint)cond << (int)CPU.Instruction.COND_SHIFT;
                    instruction |= (uint)opcode << (int)CPU.Instruction.OPCODE_SHIFT;

                    for(int i = 0; i < operands.Count; i++) {
                        switch(i) {
                            case 0:
                                instruction |= (uint)((uint)operands[i] << (int)CPU.Instruction.OP1_SHIFT);
                                instruction |= (uint)CPU.Instruction.OP1_FLAG_MASK;
                                break;
                            case 1:
                                instruction |= (uint)((uint)operands[i] << (int)CPU.Instruction.OP2_SHIFT);
                                instruction |= (uint)CPU.Instruction.OP2_FLAG_MASK;
                                break;
                            case 2:
                                instruction |= (uint)((uint)operands[i] << (int)CPU.Instruction.OP3_SHIFT);
                                instruction |= (uint)CPU.Instruction.OP3_FLAG_MASK;
                                break;
                        }
                    }

                    instruction |= immediate.var.val32.Uint;

                    return instruction;
                }
            }

            public bool Compile(string code, int memorySize) {
                Reset();

                if(!Preprocess(code)) {
                    return false;
                }

                if(!Tokenize()) {
                    return false;
                }

                if(!Parse()) {
                    return false;
                }

                if(!symbols.ContainsKey("main")) {
                    errors.Add($"program must define \"main:\" entry point");
                    return false;
                }

                if(!GenerateCode()) {
                    return false;
                }

                if(memorySize < programData.Count) {
                    errors.Add($"program data is larger than desired memory size ({memorySize} < {programData.Count})");
                    return false;
                }

                memory = new byte[memorySize];
                programData.CopyTo(memory, 0);

                return true;
            }

            static bool TryStringToOpcode(string str, out CPU.Opcode opcode) {
                var names = Enum.GetNames(typeof(CPU.Opcode));
                var values = Enum.GetValues(typeof(CPU.Opcode));
                str = str.ToUpperInvariant();
            
                for(int i = 0; i < names.Length; i++) {
                    if(names[i] == str) {
                        opcode = (CPU.Opcode)values.GetValue(i);
                        return true;
                    }
                }

                opcode = 0;
                return false;
            }

            static bool TryStringToCond(string str, out CPU.Cond cond) {
                var names = Enum.GetNames(typeof(CPU.Cond));
                var values = Enum.GetValues(typeof(CPU.Cond));
                str = str.ToUpperInvariant();
            
                for(int i = 0; i < names.Length; i++) {
                    if(names[i] == str) {
                        cond = (CPU.Cond)values.GetValue(i);
                        return true;
                    }
                }

                cond = 0;
                return false;
            }

            void AllocateRegisters() {
                var names = Enum.GetNames(typeof(CPU.Register));

                for(int i = 0; i < names.Length; i++) {
                    var name = names[i].ToLowerInvariant();

                    symbols.Add(name, new Symbol {
                        name = name,
                        var = new Variable{ val32 = new CPU.Value32{ Int = i } },
                        type = Symbol.Type.REGISTER
                    });
                }
            }

            void AddData(Variable var, int size) {
                //todo: don't ignore size
                programData.Add(var.val32.byte0);
                programData.Add(var.val32.byte1);
                programData.Add(var.val32.byte2);
                programData.Add(var.val32.byte3);
            }

            static uint GetMaxImmediateValue(int argNum) {
                switch(argNum) {
                    case 1:
                        return (uint)CPU.Instruction.IMM1_MASK;
                    case 2:
                        return (uint)CPU.Instruction.IMM2_MASK;
                    case 3:
                        return (uint)CPU.Instruction.IMM3_MASK;
                }

                return 0;
            }

            void AddError(int line, string error) {
                errors.Add($"[error] line {line}: {error}");
            }

            bool GenerateCode() {
                numInstructions = 0;

                // first pass, can't update label addresses yet
                for(int i = 0; i < instructions.Count; i++) {
                    Instruction instruction = instructions[i];
                    int additionalInstructions = instruction.AdditionalInstructions(true);

                    if(additionalInstructions != 0) {
                        instruction.additionalInstructions = new uint[additionalInstructions];

                        //todo: support more than int, uint, and float
                        instruction.additionalInstructions[0] = instruction.immediate.var.val32.Uint;
                        instruction.immediate.var.val32.Uint = GetMaxImmediateValue(instruction.operands.Count + 1);
                    }

                    instruction.address = numInstructions;
                    numInstructions += 1 + additionalInstructions;

                    instructions[i] = instruction;
                }

                int growth = 0;

                for(int i = 0; i < instructions.Count; i++) {
                    Instruction instruction = instructions[i];
                
                    if(instruction.immediate.type == Symbol.Type.LABEL) {
                        uint maxValue = GetMaxImmediateValue(instruction.operands.Count + 1);
                        var immediate = instruction.immediate;
                        Instruction target = instructions[instruction.immediate.labelInstructionIndex];

                        if(target.address + growth >= maxValue) {
                            growth++;
                            numInstructions++;
                            instruction.additionalInstructions = new uint[1];
                            instruction.additionalInstructions[0] = (uint)(target.address + growth);
                            instruction.immediate.var.val32.Uint = maxValue;
                        } else {
                            instruction.immediate.var.val32.Int = target.address + growth;
                        }

                        if(!symbols.ContainsKey(instruction.immediate.name)) {
                            errors.Add($"missing symbol \"{instruction.immediate.name}\" (this should never happen)");
                            return false;
                        }

                        var symbol = symbols[instruction.immediate.name];
                        symbol.var.val32.Int = target.address + growth;
                        symbols[instruction.immediate.name] = symbol;
                    }

                    instructions[i] = instruction;

                    //Print($"{instruction.opcode} [{String.Join(", ", instruction.operands)}] {instruction.immediate.word.Uint} [{(instruction.additionalInstructions == null ? "" : String.Join(", ", instruction.additionalInstructions))}]");
                }

                List<KeyValuePair<string, Symbol>> changes = new List<KeyValuePair<string, Symbol>>();

                foreach(var pair in symbols) {
                    Symbol symbol = pair.Value;

                    if(symbol.type == Symbol.Type.LABEL) {
                        int addr = instructions[symbol.labelInstructionIndex].address;

                        if(symbol.var.val32.Int != addr) {
                            symbol.var.val32.Int = addr;
                            changes.Add(new KeyValuePair<string, Symbol>(pair.Key, symbol));
                        }
                    }
                }

                foreach(var pair in changes) {
                    symbols[pair.Key] = pair.Value;
                }

                foreach(var pair in isrs) {
                    Symbol target = pair.Key;
                    Symbol replacement = pair.Value;
                    Instruction targetInstruction = instructions[target.labelInstructionIndex];
                    Instruction replacementInstruction = instructions[replacement.labelInstructionIndex];

                    if(targetInstruction.additionalInstructions != null) {
                        errors.Add($"isr \"{target.name}\" is broken, stub address is too large");
                        return false;
                    } else if(replacementInstruction.address >= GetMaxImmediateValue(1)) {
                        errors.Add($"isr \"{replacement.name}\" address is too large");
                        return false;
                    }

                    targetInstruction.immediate.var.val32.Int = replacementInstruction.address;
                    instructions[target.labelInstructionIndex] = targetInstruction;
                }

                return true;
            }

            bool Parse() {
                AllocateRegisters();
                numInstructions = 0;

                for(int i = 0; i < statements.Count; i++) {
                    var statement = statements[i];

                    if(statement.tokens[0].type == Token.Type.LABEL) {
                        if(symbols.ContainsKey(statement.tokens[0].stringValue)) {
                            AddError(statement.lineNum, $"redefinition of identifier \"{statement.tokens[0].stringValue}\"");
                            return false;
                        } else {
                            symbols.Add(statement.tokens[0].stringValue, new Symbol {
                                name = statement.tokens[0].stringValue,
                                var = new Variable {
                                    type = Variable.Type.NONE
                                },
                                labelInstructionIndex = numInstructions,
                                type = Symbol.Type.LABEL
                            });
                        }
                    } else if(statement.tokens[0].type == Token.Type.INSTRUCTION) {
                        numInstructions++;
                    }
                }

                for(int i = 0; i < statements.Count; i++) {
                    var statement = statements[i];

                    if(statement.tokens[0].type == Token.Type.DIRECTIVE) {
                        if(statement.tokens[0].stringValue == "const" || statement.tokens[0].stringValue == "word") {
                            if(statement.tokens.Length != 3 || statement.tokens[1].type != Token.Type.IDENTIFIER || (statement.tokens[2].type != Token.Type.INTEGER && statement.tokens[2].type != Token.Type.FLOAT)) {
                                AddError(statement.lineNum, $"invalid directive");
                                return false;
                            }

                            if(symbols.ContainsKey(statement.tokens[1].stringValue)) {
                                AddError(statement.lineNum, $"redefinition of identifier \"{statement.tokens[1].stringValue}\"");
                                return false;
                            } else {
                                if(statement.tokens[0].stringValue == "const") {
                                    symbols.Add(statement.tokens[1].stringValue, new Symbol {
                                        name = statement.tokens[1].stringValue,
                                        var = statement.tokens[2].var,
                                        type = Symbol.Type.CONSTANT
                                    });
                                } else {
                                    int addr = programData.Count;
                                    AddData(statement.tokens[2].var, 4);

                                    symbols.Add(statement.tokens[1].stringValue, new Symbol {
                                        name = statement.tokens[1].stringValue,
                                        var = new Variable{ val32 = new CPU.Value32{ Int = addr }},
                                        type = Symbol.Type.CONSTANT
                                    });
                                }
                            }
                        } else if(statement.tokens[0].stringValue == "isr") {
                            if(statement.tokens.Length != 3 || statement.tokens[1].type != Token.Type.IDENTIFIER ||statement.tokens[2].type != Token.Type.IDENTIFIER) {
                                AddError(statement.lineNum, $"invalid directive");
                                return false;
                            }

                            Symbol target;
                            Symbol replacement;

                            if(!symbols.TryGetValue(statement.tokens[1].stringValue, out target)) {
                                AddError(statement.lineNum, $"invalid isr directive, no symbol \"{statement.tokens[1].stringValue}\"");
                                return false;
                            } else if(target.type != Symbol.Type.LABEL) {
                                AddError(statement.lineNum, $"invalid isr directive, symbol \"{statement.tokens[1].stringValue}\" is not a label");
                                return false;
                            }

                            if(!symbols.TryGetValue(statement.tokens[2].stringValue, out replacement)) {
                                AddError(statement.lineNum, $"invalid isr directive, no symbol \"{statement.tokens[2].stringValue}\"");
                                return false;
                            } else if(replacement.type != Symbol.Type.LABEL) {
                                AddError(statement.lineNum, $"invalid isr directive, symbol \"{statement.tokens[2].stringValue}\" is not a label");
                                return false;
                            }

                            isrs.Add(new KeyValuePair<Symbol, Symbol>(target, replacement));
                        } else {
                            AddError(statement.lineNum, $"unknown directive \"{statement.tokens[0].stringValue}\"");
                            return false;
                        }
                    } else if(statement.tokens[0].type == Token.Type.INSTRUCTION) {
                        var instruction = new Instruction {
                            opcode = 0,
                            cond = 0,
                            operands = new List<CPU.Register>(),
                            address = 0,
                            additionalInstructions = null,
                            immediate = new Symbol {
                                var = new Variable{ type = Variable.Type.NONE },
                                type = Symbol.Type.NONE
                            }
                        };

                        if(!TryStringToOpcode(statement.tokens[0].stringValue, out instruction.opcode)) {
                            AddError(statement.lineNum, $"unknown opcode \"{statement.tokens[0].stringValue}\"");
                            return false;
                        }

                        if(!TryStringToCond(statement.tokens[0].cond, out instruction.cond)) {
                            AddError(statement.lineNum, $"unknown condition \"{statement.tokens[0].cond}\"");
                            return false;
                        }

                        for(int j = 1; j < statement.tokens.Length; j++) {
                            if(statement.tokens[j].type == Token.Type.IDENTIFIER) {
                                Symbol symbol;

                                if(!symbols.TryGetValue(statement.tokens[j].stringValue, out symbol)) {
                                    AddError(statement.lineNum, $"unknown identifier \"{statement.tokens[j].stringValue}\"");
                                    return false;
                                }

                                if(symbol.type == Symbol.Type.REGISTER) {
                                    instruction.operands.Add((CPU.Register)symbol.var.val32.Uint);
                                } else {
                                    instruction.immediate = symbol;
                                }
                            } else if(statement.tokens[j].type == Token.Type.INTEGER || statement.tokens[j].type == Token.Type.FLOAT) {
                                instruction.immediate = new Symbol {
                                    var = statement.tokens[j].var,
                                    type = Symbol.Type.LITERAL
                                };
                            } else {
                                AddError(statement.lineNum, $"invalid instruction argument \"{statement.tokens[j].stringValue}\"");
                                return false;
                            }
                        }

                        instructions.Add(instruction);
                    }
                }

                return true;
            }

            static bool TryParseIntegerLiteral(string str, out int value) {
                Match decimalMatch = Regex.Match(str, @"^[+-]?[0-9]+$");
                Match hexMatch = Regex.Match(str, @"^([+-])?0x([0-9a-zA-Z]+)$");
                Match binMatch = Regex.Match(str, @"^([+-])?0b([01]+)$");

                if(decimalMatch.Success) {
                    value = Convert.ToInt32(decimalMatch.Groups[0].ToString(), 10);
                    return true;
                } else if(hexMatch.Success) {
                    str = hexMatch.Groups[0].ToString();
                    value = Convert.ToInt32(hexMatch.Groups[2].ToString(), 16);

                    if(hexMatch.Groups[1].ToString() == "-") {
                        value *= -1;
                    }

                    return true;
                } else if(binMatch.Success) {
                    str = binMatch.Groups[0].ToString();
                    value = Convert.ToInt32(binMatch.Groups[2].ToString(), 2);

                    if(binMatch.Groups[1].ToString() == "-") {
                        value *= -1;
                    }

                    return true;
                }

                value = 0;
                return false;
            }

            bool Tokenize() {       
                for(int i = 0; i < statements.Count; i++) {
                    var statement = statements[i];

                    for(int j = 0; j < statement.line.Length; j++) {
                        string arg = statement.line[j];

                        statement.tokens[j].offset = 0;
                        statement.tokens[j].type = Token.Type.NONE;

                        Match directiveMatch = Regex.Match(arg, @"^\.([a-zA-Z_][a-zA-Z0-9_]*)$");
                        Match labelMatch = Regex.Match(arg, @"^([a-zA-Z_][a-zA-Z0-9_]*):$");
                        Match identifierMatch = Regex.Match(arg, @"^[a-zA-Z_][a-zA-Z0-9_]*$");
                        Match instructionMatch = Regex.Match(arg, @"^([a-zA-Z_][a-zA-Z0-9_]*)\.(al|eq|ne|gt|ge|lt|le)$");
                        Match floatMatch = Regex.Match(arg, @"^[+-]?[0-9]?[\.][0-9]*$");

                        if(directiveMatch.Success) {
                            statement.tokens[j].type = Token.Type.DIRECTIVE;
                            statement.tokens[j].stringValue = directiveMatch.Groups[1].ToString();
                        } else if(labelMatch.Success) {
                            statement.tokens[j].type = Token.Type.LABEL;
                            statement.tokens[j].stringValue = labelMatch.Groups[1].ToString();
                        } else if(identifierMatch.Success) {
                            statement.tokens[j].type = (j == 0) ? Token.Type.INSTRUCTION : Token.Type.IDENTIFIER;
                            statement.tokens[j].stringValue = identifierMatch.Groups[0].ToString();

                            if(statement.tokens[j].type == Token.Type.INSTRUCTION) {
                                statement.tokens[j].cond = "al";
                            }
                        } else if(instructionMatch.Success) {
                            statement.tokens[j].type = Token.Type.INSTRUCTION;
                            statement.tokens[j].stringValue = instructionMatch.Groups[1].ToString();
                            statement.tokens[j].cond = instructionMatch.Groups[2].ToString();
                        } else if(floatMatch.Success && floatMatch.Groups[0].ToString() != ".") {
                            statement.tokens[j].type = Token.Type.FLOAT;
                            statement.tokens[j].stringValue = floatMatch.Groups[0].ToString();
                            float.TryParse(statement.tokens[j].stringValue, out statement.tokens[j].var.val32.Float);
                            statement.tokens[j].var.type = Variable.Type.FLOAT;
                        } else if(TryParseIntegerLiteral(arg, out statement.tokens[j].var.val32.Int)) {
                            statement.tokens[j].type = Token.Type.INTEGER;
                            statement.tokens[j].stringValue = arg;
                            statement.tokens[j].var.type = Variable.Type.INT;
                        }
                    }
                }

                return true;
            }

            bool Preprocess(string code) {
                var lines = code.Split(new[] {"\r\n", "\r", "\n"}, StringSplitOptions.None);
            
                for(int i = 0; i < lines.Length; i++) {
                    lines[i] = code = Regex.Replace(lines[i].Trim() ,@"\s+"," ");

                    if(lines[i].Length > 0) {
                        if(lines[i][0] == '#') {
                            continue;
                        }

                        var args = lines[i].Split(' ');

                        if(args.Length > 0) {
                            statements.Add(new Statement {
                                lineNum = i,
                                line = args,
                                tokens = new Token[args.Length]
                            });
                        }
                    }
                }

                return true;
            }
        }

        class CPU {
            public uint[] registers = new uint[64];
            public byte[] memory = null;
            public uint[] instructions = null;
            public uint flags = (uint)Flag.INTERRUPTS_ENABLED;
            Queue<uint> pendingInterrupts = new Queue<uint>();
            public IPeripheral peripheral = null;
            static int maxPendingInterrupts = 32;
            System.Random random = new System.Random();
            public uint pc = 0;
            public uint peripheralBase = 0;
            Status savedStatus;

            static void Print(string msg) {
                Console.WriteLine(msg);
            }

            static void PrintVar<T>(string name, T var) {
                Print($"{name}: {var}");
            }

            public CPU(IPeripheral peripheral, uint peripheralBase) {
                this.peripheral = peripheral;
                this.peripheralBase = peripheralBase;
            }

            public void Reset() {
                registers = new uint[64];
                memory = null;
                instructions = null;
                flags = (uint)Flag.INTERRUPTS_ENABLED;
                pendingInterrupts.Clear();
            }

            public interface IPeripheral {
                Value32 Read(uint addr);
                void Write(uint addr, Value32 value);
            }

            [Flags]
            public enum Flag : uint {
                INTERRUPTS_ENABLED = 1 << 0,
                EQUAL = 1 << 1,
                GREATER_THAN = 1 << 2,
                LESS_THAN = 1 << 3,
                READY = 1 << 4
            }

            [StructLayout(LayoutKind.Explicit)]
            public struct Value32 {
                [FieldOffset(0)]
                public int Int;
                [FieldOffset(0)]
                public uint Uint;
                [FieldOffset(0)]
                public float Float;
                [FieldOffset(0)]
                public byte byte0;
                [FieldOffset(1)]
                public byte byte1;
                [FieldOffset(2)]
                public byte byte2;
                [FieldOffset(3)]
                public byte byte3;
            }

            public enum Instruction {
                COND_SHIFT = 29,
                COND_MASK = 7 << (int)COND_SHIFT,
                OPCODE_SHIFT = 23,
                OPCODE_MASK = 0x3F << (int)OPCODE_SHIFT,
                OP1_FLAG_SHIFT = 22,
                OP1_FLAG_MASK = 1 << (int)OP1_FLAG_SHIFT,
                OP1_SHIFT = 16,
                OP1_MASK = 0x3F << (int)OP1_SHIFT,
                OP2_FLAG_SHIFT = 15,
                OP2_FLAG_MASK = 1 << (int)OP2_FLAG_SHIFT,
                OP2_SHIFT = 9,
                OP2_MASK = 0x3F << (int)OP2_SHIFT,
                OP3_FLAG_SHIFT = 8,
                OP3_FLAG_MASK = 1 << (int)OP3_FLAG_SHIFT,
                OP3_SHIFT = 2,
                OP3_MASK = 0x3F << (int)OP3_SHIFT,
                IMM1_SHIFT = 0,
                IMM1_MASK = 0x3FFFFF << (int)IMM1_SHIFT,
                IMM2_SHIFT = 0,
                IMM2_MASK = 0x7FFF << (int)IMM2_SHIFT,
                IMM3_SHIFT = 0,
                IMM3_MASK = 0xFF << (int)IMM3_SHIFT,
                IMM4_SHIFT = 0,
                IMM4_MASK = 0x3 << (int)IMM4_SHIFT
            }
        
            public enum Cond {
                AL, EQ, NE, GT, GE, LT, LE, RESERVED
            }

            public enum Register {
                R0, R1, R2,  R3,  R4,  R5,  R6,  R7,
                R8, R9, R10, R11, R12, R13, R14, R15,
                SP, BP
            }

            public enum Opcode {
                RET, CLI, SEI,
                JMP, JNE, CALL, PUSH, POP,
                MOV, LDR, LDRB, STR, STRB, CMPI, CMPU,
                SHRS, SHRU, SHL, AND, OR, XOR, NOT, ADD, SUB, MUL, DIV, MOD,
                ITOF, FTOI, CMPF, ADDF, SUBF, MULF, DIVF, MODF,
                RESERVED = 0x3F
            }

            public enum Status {
                SUCCESS,
                OUT_OF_INSTRUCTIONS,
                MISSING_INSTRUCTION,
                BAD_INSTRUCTION,
                SEGFAULT,
                DIVISION_BY_ZERO,
                UNDEFINED
            }

            public bool Interrupt(uint addr) {
                if((flags & (uint)Flag.READY) == 0) {
                    return false;
                }

                if(pendingInterrupts.Count >= maxPendingInterrupts) {
                    return false;
                }

                pendingInterrupts.Enqueue(addr);
                return true;
            }

            void AssignMemory(uint addr, Value32 val) {
                //Print($"AssignMemory(uint addr: {addr}, uint value: {value})");

                if(peripheral != null && addr >= peripheralBase) {
                    peripheral.Write(addr, val);
                    return;
                } else if(addr >= memory.Length) {
                    savedStatus = Status.SEGFAULT;
                    return;
                }

                memory[addr + 0] = val.byte0;
                memory[addr + 1] = val.byte1;
                memory[addr + 2] = val.byte2;
                memory[addr + 3] = val.byte3;
            }

            void AssignMemory(uint addr, uint val) {
                AssignMemory(addr, new Value32 { Uint = val });
            }

            Value32 ReadMemory(uint addr) {
                if(peripheral != null && addr >= peripheralBase) {
                    return peripheral.Read(addr);
                } else if(addr >= memory.Length) {
                    savedStatus = Status.SEGFAULT;
                    return new Value32 { Uint = 0 };
                }

                Value32 val = new Value32 {
                    byte0 = memory[addr + 0],
                    byte1 = memory[addr + 1],
                    byte2 = memory[addr + 2],
                    byte3 = memory[addr + 3]
                };

                //Print($"ReadMemory(uint addr: {addr}) -> uint: {val.Uint}");
                return val;
            }

            public bool Cycle(out Status status, int numCycles = 1) {
                savedStatus = Status.UNDEFINED;

                for(int i = 0; i < numCycles; i++) {
                    if(savedStatus != Status.UNDEFINED) {
                        status = savedStatus;
                        return false;
                    }

                    if((flags & (uint)Flag.INTERRUPTS_ENABLED) != 0 && pendingInterrupts.Count != 0) {
                        uint addr = pendingInterrupts.Dequeue();

                        AssignMemory(registers[(int)Register.SP], pc);
                        registers[(int)Register.SP] += 4;
                        pc = addr;

                        if(savedStatus != Status.UNDEFINED) {
                            status = savedStatus;
                            return false;
                        }
                    }

                    if(pc >= instructions.Length) {
                        status = Status.OUT_OF_INSTRUCTIONS;
                        return false;
                    }

                    uint inst = instructions[pc++];
                    Opcode opcode = (Opcode)((inst & (uint)Instruction.OPCODE_MASK) >> (int)Instruction.OPCODE_SHIFT);
                    Cond cond = (Cond)((inst & (int)Instruction.COND_MASK) >> (int)Instruction.COND_SHIFT);

                    /*PrintVar(nameof(pc), pc);
                    Print($"instruction: {opcode}.{cond}");
                    Print($"instruction bits: {Convert.ToString(inst, 2).PadLeft(32, '0')}");
                    Print($"flags: {Convert.ToString(flags & (uint)Flag.EQUAL, 2).PadLeft(32, '0')}");*/

                    switch(cond) {
                        case Cond.EQ:
                            if((flags & (uint)Flag.EQUAL) != 0) {
                                break;
                            }

                            continue;
                        case Cond.NE:
                            if((flags & (uint)Flag.EQUAL) == 0) {
                                break;
                            }

                            continue;
                        case Cond.GT:
                            if((flags & (uint)Flag.GREATER_THAN) != 0) {
                                break;
                            }

                            continue;
                        case Cond.LT:
                            if((flags & (uint)Flag.LESS_THAN) != 0) {
                                break;
                            }

                            continue;
                        case Cond.GE:
                            if((flags & (uint)(Flag.GREATER_THAN | Flag.EQUAL)) != 0) {
                                break;
                            }

                            continue;
                        case Cond.LE:
                            if((flags & (uint)(Flag.LESS_THAN | Flag.EQUAL)) != 0) {
                                break;
                            }

                            continue;
                    }

                    bool handledHere = true;

                    // zero arg instructions
                    switch(opcode) {
                        case Opcode.RET:
                            registers[(int)Register.SP] -= 4;
                            pc = ReadMemory(registers[(int)Register.SP]).Uint;
                            break;
                        case Opcode.CLI:
                            flags &= ~(uint)Flag.INTERRUPTS_ENABLED;
                            break;
                        case Opcode.SEI:
                            flags |= (uint)Flag.INTERRUPTS_ENABLED;
                            break;
                        default:
                            handledHere = false;
                            break;
                    }

                    if(handledHere) {
                        if(savedStatus == Status.UNDEFINED) {
                            status = Status.SUCCESS;
                            continue;
                        } else {
                            status = savedStatus;
                            return false;
                        }
                    }

                    uint op1 = (inst & (uint)Instruction.OP1_MASK) >> (int)Instruction.OP1_SHIFT;
                    uint op1Flag = inst & (uint)Instruction.OP1_FLAG_MASK;
                    uint imm1 = (inst & (uint)Instruction.IMM1_MASK) >> (int)Instruction.IMM1_SHIFT;

                    if(op1Flag == 0 && imm1 == (uint)Instruction.IMM1_MASK) {
                        if(pc >= instructions.Length) {
                            status = Status.OUT_OF_INSTRUCTIONS;
                            return false;
                        }

                        imm1 = instructions[pc++];
                    }

                    uint arg1 = (op1Flag != 0) ? registers[op1] : imm1;
                    // just testing this, if it's not slower, arg1 will just be a Value32
                    Value32 arg1v = new Value32 { Uint = arg1 };
                    handledHere = true;

                    /*PrintVar(nameof(op1), op1);
                    PrintVar(nameof(op1Flag), op1Flag);
                    PrintVar(nameof(imm1), imm1);
                    PrintVar(nameof(arg1), arg1);*/

                    // one arg instructions
                    switch(opcode) {
                        case Opcode.JMP:
                            pc = arg1;
                            break;
                        case Opcode.CALL:
                            AssignMemory(registers[(int)Register.SP], pc);
                            registers[(int)Register.SP] += 4;
                            pc = arg1;
                            break;
                        case Opcode.PUSH:
                            AssignMemory(registers[(int)Register.SP], arg1);
                            registers[(int)Register.SP] += 4;
                            break;
                        case Opcode.POP:
                            registers[(int)Register.SP] -= 4;
                            registers[op1] = ReadMemory(registers[(int)Register.SP]).Uint;
                            break;
                        case Opcode.ITOF:
                            var itof = new Value32 { Uint = registers[op1] };
                            itof.Float = (float)itof.Int;
                            registers[op1] = itof.Uint;
                            break;
                        case Opcode.FTOI:
                            var ftoi = new Value32 { Uint = registers[op1] };
                            ftoi.Int = (int)ftoi.Float;
                            registers[op1] = ftoi.Uint;
                            break;
                        default:
                            handledHere = false;
                            break;
                    }

                    if(handledHere) {
                        continue;
                    }

                    uint op2 = (inst & (uint)Instruction.OP2_MASK) >> (int)Instruction.OP2_SHIFT;
                    uint op2Flag = inst & (uint)Instruction.OP2_FLAG_MASK;
                    uint imm2 = (inst & (uint)Instruction.IMM2_MASK) >> (int)Instruction.IMM2_SHIFT;

                    if(op2Flag == 0 && imm2 == (uint)Instruction.IMM2_MASK) {
                        if(pc >= instructions.Length) {
                            status = Status.OUT_OF_INSTRUCTIONS;
                            return false;
                        }

                        imm2 = instructions[pc++];
                    }

                    uint arg2 = (op2Flag != 0) ? registers[op2] : imm2;
                    Value32 arg2v = new Value32 { Uint = arg2 };
                    handledHere = true;

                    /*PrintVar(nameof(op2), op2);
                    PrintVar(nameof(op2Flag), op2Flag);
                    PrintVar(nameof(imm2), imm2);
                    PrintVar(nameof(arg2), arg2);*/

                    // two arg instructions
                    switch(opcode) {
                        case Opcode.MOV:
                            registers[op1] = arg2;
                            break;
                        case Opcode.LDR:
                            registers[op1] = ReadMemory(arg2).Uint;
                            break;
                        case Opcode.STR:
                            AssignMemory(arg2, arg1);
                            break;
                        case Opcode.CMPI:
                            flags = ((int)arg1 == (int)arg2) ? flags | (uint)Flag.EQUAL : flags & ~(uint)Flag.EQUAL;
                            flags = ((int)arg1 > (int)arg2) ? flags | (uint)Flag.GREATER_THAN : flags & ~(uint)Flag.GREATER_THAN;
                            flags = ((int)arg1 < (int)arg2) ? flags | (uint)Flag.LESS_THAN : flags & ~(uint)Flag.LESS_THAN;
                            break;
                        case Opcode.CMPU:
                            flags = (arg1 == arg2) ? flags | (uint)Flag.EQUAL : flags & ~(uint)Flag.EQUAL;
                            flags = (arg1 > arg2) ? flags | (uint)Flag.GREATER_THAN : flags & ~(uint)Flag.GREATER_THAN;
                            flags = (arg1 < arg2) ? flags | (uint)Flag.LESS_THAN : flags & ~(uint)Flag.LESS_THAN;
                            break;
                        case Opcode.CMPF:
                            flags = (arg1v.Float == arg2v.Float) ? flags | (uint)Flag.EQUAL : flags & ~(uint)Flag.EQUAL;
                            flags = (arg1v.Float > arg2v.Float) ? flags | (uint)Flag.GREATER_THAN : flags & ~(uint)Flag.GREATER_THAN;
                            flags = (arg1v.Float < arg2v.Float) ? flags | (uint)Flag.LESS_THAN : flags & ~(uint)Flag.LESS_THAN;
                            break;
                        default:
                            handledHere = false;
                            break;
                    }

                    if(handledHere) {
                        continue;
                    }

                    uint op3 = (inst & (uint)Instruction.OP3_MASK) >> (int)Instruction.OP3_SHIFT; 
                    uint op3Flag = inst & (uint)Instruction.OP3_FLAG_MASK;
                    uint imm3 = (inst & (uint)Instruction.IMM3_MASK) >> (int)Instruction.IMM3_SHIFT;

                    if(op3Flag == 0 && imm3 == (uint)Instruction.IMM3_MASK) {
                        if(pc >= instructions.Length) {
                            status = Status.OUT_OF_INSTRUCTIONS;
                            return false;
                        }

                        imm3 = instructions[pc++];
                    }

                    uint arg3 = (op3Flag != 0) ? registers[op3] : imm3;
                    Value32 arg3v = new Value32 { Uint = arg3 };
                    handledHere = true;

                    /*PrintVar(nameof(op3), op3);
                    PrintVar(nameof(op3Flag), op3Flag);
                    PrintVar(nameof(imm3), imm3);
                    PrintVar(nameof(arg3), arg3);*/

                    // three arg instructions
                    switch(opcode) {
                        case Opcode.SHRS:
                            registers[op1] = (uint)((int)arg2 >> (int)arg3);
                            break;
                        case Opcode.SHRU:
                            registers[op1] = arg2 >> (int)arg3;
                            break;
                        case Opcode.SHL:
                            registers[op1] = arg2 << (int)arg3;
                            break;
                        case Opcode.AND:
                            registers[op1] = arg2 & arg3;
                            break;
                        case Opcode.OR:
                            registers[op1] = arg2 | arg3;
                            break;
                        case Opcode.XOR:
                            registers[op1] = arg2 ^ arg3;
                            break;
                        case Opcode.NOT:
                            registers[op1] = ~arg2;
                            break;
                        case Opcode.ADD:
                            registers[op1] = arg2 + arg3;
                            break;
                        case Opcode.SUB:
                            registers[op1] = arg2 - arg3;
                            break;
                        case Opcode.MUL:
                            registers[op1] = arg2 * arg3;
                            break;
                        case Opcode.DIV:
                            if(arg2 == 0) {
                                status = Status.DIVISION_BY_ZERO;
                                return false;
                            }

                            registers[op1] = arg2 / arg3;
                            break;
                        case Opcode.MOD:
                            if(arg2 == 0) {
                                status = Status.DIVISION_BY_ZERO;
                                return false;
                            }

                            registers[op1] = arg2 % arg3;
                            break;
                        case Opcode.ADDF:
                            registers[op1] = new Value32 { Float = arg2v.Float + arg3v.Float }.Uint;
                            break;
                        case Opcode.SUBF:
                            registers[op1] = new Value32 { Float = arg2v.Float - arg3v.Float }.Uint;
                            break;
                        case Opcode.MULF:
                            registers[op1] = new Value32 { Float = arg2v.Float * arg3v.Float }.Uint;
                            break;
                        case Opcode.DIVF:
                            if(arg2v.Float == 0) {
                                status = Status.DIVISION_BY_ZERO;
                                return false;
                            }

                            registers[op1] = new Value32 { Float = arg2v.Float / arg3v.Float }.Uint;
                            break;
                        case Opcode.MODF:
                            if(arg2v.Float == 0) {
                                status = Status.DIVISION_BY_ZERO;
                                return false;
                            }

                            registers[op1] = new Value32 { Float = arg2v.Float % arg3v.Float }.Uint;
                            break;
                        default:
                            handledHere = false;
                            break;
                    }

                    if(handledHere) {
                        continue;
                    }

                    status = Status.MISSING_INSTRUCTION;
                    return false;
                }

                if(savedStatus != Status.UNDEFINED) {
                    status = savedStatus;
                    return false;
                } else {
                    status = Status.SUCCESS;
                    return true;
                }
            }
        }

        struct IOEntityMapper {
            public ulong mcuId;
            public int index;
        }

        static void Print(string msg) {
            Interface.Oxide.LogInfo(msg, new object[]{});
        }

        static void PrintVar<T>(string name, T var) {
            Print($"{name}: {var}");
        }

        void Init() {
            config = Config.ReadObject<ConfigData>();
            plugin = this;
            assembler = new Assembler();

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
            assembler = null;
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

            // bind p "give note 10;env.time 12;god true;give hammer;give planner;give wood 5000;give stones 5000;give wiretool;give electric.solarpanel.large;give electric.random.switch;give ceilinglight 4"
            /*bool success = assembler.Compile(comp.setupCode + @"
            out channel 0
            out output_energy 2
            out channel 1
            out output_energy 2
            out channel 2
            out output_energy 2
            out channel 3
            out output_energy 2

            loop:
                rngi r0
                out output_mask r0
                jmp loop
            ", 1024);

            if(success) {
                comp.cpu.LoadProgram(assembler.instructions.ToArray(), assembler.memory, assembler.programData.Count);
            } else {
                foreach(var error in assembler.errors) {
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
            static float startTime = Time.realtimeSinceStartup;

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

                    if(comp.wantsUpdate && Time.realtimeSinceStartup > comp.lastUpdateTime + IOEntity.responsetime) {
                        comp.UpdateMaskedOutput();
                    }
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
            public int[] maskedOutputEnergies = new int[numChannels];
            public CPU cpu = null;
            public BaseEntity stash = null;
            public float lastUpdateTime = 0;
            public bool wantsUpdate = false;
            public string setupCode = @"
            .const channel 0
            .const output_energy 1
            .const input_energy 2
            .const output_mask 3
            .const update 4

            jmp user_code

            input:
                jmp input_stub

            input_stub:
                ret

            user_code:
            ";

            public McuComponent() {
                cpu = new CPU(new Peripheral(this), (uint)Peripheral.Port.BASE_ADDR);
                id = idCtr++;
            }

            class Peripheral : CPU.IPeripheral {
                McuComponent comp = null;
                int[] desiredOutputEnergies = null;
                int selectedChannel = 0;
                int outputMask = 0;

                public enum Port : uint {
                    BASE_ADDR = 0x80000000,
                    CHANNEL = BASE_ADDR,
                    OUTPUT_ENERGY = BASE_ADDR + 1,
                    INPUT_ENERGY = BASE_ADDR + 2,
                    OUTPUT_MASK = BASE_ADDR + 3,
                    UPDATE = BASE_ADDR + 4
                }
                
                public Peripheral(McuComponent comp) {
                    this.comp = comp;
                    desiredOutputEnergies = new int[comp.outputEnergies.Length];
                }

                public void UpdateMaskedOutputEnergies() {
                    for(int i = 0; i < desiredOutputEnergies.Length; i++) {
                        comp.maskedOutputEnergies[i] = ((outputMask & (1 << i)) != 0) ? desiredOutputEnergies[i] : 0;
                    }

                    comp.UpdateMaskedOutput();
                }

                public CPU.Value32 Read(uint addr) {
                    switch((Port)addr) {
                        case Port.CHANNEL:
                            return new CPU.Value32 { Int = selectedChannel };
                        case Port.OUTPUT_ENERGY:
                            return new CPU.Value32 { Int = comp.outputEnergies[selectedChannel] };
                        case Port.INPUT_ENERGY:
                            return new CPU.Value32 { Int = comp.inputEnergies[selectedChannel] };
                        case Port.OUTPUT_MASK:
                            return new CPU.Value32 { Int = outputMask };
                    }

                    return new CPU.Value32 { Int = 0 };
                }

                public void Write(uint addr, CPU.Value32 value) {
                    switch((Port)addr) {
                        case Port.CHANNEL:
                            selectedChannel = Math.Max(0, Math.Min(value.Int, desiredOutputEnergies.Length - 1));
                            break;
                        case Port.OUTPUT_ENERGY:
                            desiredOutputEnergies[selectedChannel] = value.Int;
                            break;
                        case Port.OUTPUT_MASK:
                            outputMask = value.Int;
                            break;
                        case Port.UPDATE:
                            UpdateMaskedOutputEnergies();
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

            public void UpdateMaskedOutput() {
                UpdateOutputEnergies(maskedOutputEnergies);
            }

            public void UpdateOutputEnergies(int[] energies) {
                if(Time.realtimeSinceStartup <= lastUpdateTime + IOEntity.responsetime) {
                    wantsUpdate = true;
                    return;
                }

                wantsUpdate = false;
                lastUpdateTime = Time.realtimeSinceStartup;
                
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
                    cpu.Interrupt(1);
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
                                    bool success = assembler.Compile(setupCode + item.text, 1024);

                                    if(!success) { 
                                        item.text += "\n" + String.Join("\n", assembler.errors.Select(x => {
                                            return string.Format(plugin.lang.GetMessage("compiler_error", plugin), x);
                                        }));
                                    } else {
                                        Print("loaded instructions");
                                        assembler.LoadProgramToCPU(cpu);
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
                CPU.Status status;

                while(numInstructionsExecuted < config.maxInstructionsPerCycle && (cpu.flags & (uint)CPU.Flag.READY) != 0) {
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