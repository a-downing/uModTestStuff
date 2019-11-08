import fs from 'fs'

let config = require('./robopaste.config.js')
let keys = Object.keys(config)

for(let i = 0; i < keys.length; i++) {
    let filename = keys[i]
    let source = fs.readFileSync(filename, 'utf8')
    let regex = config[filename]
    let match = source.match(config[filename].start)

    if(match && match.index) {
        let copy = getMatch(match, source)
        let copyIndentation = getIndentation(match.index - 1, source)
        let target = fs.readFileSync(config[filename].target.filename, 'utf8')
        match = target.match(config[filename].target.start)

        if(match && match.index) {
            let paste = getMatch(match, target)
            let pasteIndentation = getIndentation(match.index - 1, target)
            let copyLines = copy.split('\n')
            let newSource = target.substring(0, match.index)

            for(let i = 0; i < copyLines.length; i++) {
                let copyLine = copyLines[i]
                let diff = copyIndentation - pasteIndentation
                let lineIndentation = copyLine.search(/[\S]/)
                
                if(lineIndentation != -1 && i != 0) {
                    copyLine = ' '.repeat(lineIndentation - diff) + copyLine.substring(lineIndentation)
                }

                if(i != copyLines.length - 1) {
                    newSource += copyLine + '\n'
                } else {
                    newSource += copyLine
                }
            }

            newSource += target.substring(match.index + paste.length)
            console.log(newSource)
        }  else {
            console.log(`failed to find paste "${config[filename].target.start} in "${config[filename].target.filename}"`)
        }
    } else {
        console.log(`failed to find copy "${config[filename]} in "${filename}"`)
    }
}

function getIndentation(index: number, source: string) {
    if(index == 0) {
        return 0
    }

    let c = source[index--]
    let indentation = 0

    while(index >= 0 && c != '\n') {
        c = source[index--]
        indentation++
    }

    return indentation
}

function getMatch(match: RegExpMatchArray, source: string) {
    if(match.index == null) {
        return ''
    }

    let str = match[0]
    let index = match.index + match[0].length
    let balance = 1
    let start = source[index - 1]
    let end = ''

    switch(start) {
        case '(':
            end = ')'
            break
        case '{':
            end = '}'
            break
    }
    
    while(balance > 0) {
        let c = source[index++]

        switch(c) {
            case start:
                balance++
                break
            case end:
                balance--
                break
        }

        str += c
    }

    return str
}