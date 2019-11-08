module.exports = {
    'MicroVM/MicroVM.CPU.cs': {
        start: /class CPU \{/m,
        target: {
            filename: 'Microcontroller.cs',
            start: /class CPU \{/m
        }
    }
}