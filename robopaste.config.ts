import { Replacement } from './robopaste'

let replacements: Replacement[] = [
    {source: {
        filename: 'MicroVM/MicroVM.CPU.cs',
        start: /class CPU \{/m
    },
    target: {
        filename: 'Microcontroller.cs',
        start: /class CPU \{/m
    }},

    {source: {
        filename: 'MicroVM/MicroVM.Assembler.cs',
        start: /class Assembler \{/m
    },
    target: {
        filename: 'Microcontroller.cs',
        start: /class Assembler \{/m
    }}
]

export default replacements