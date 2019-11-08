import fs from 'fs'
import config from './robopaste.config'

interface SourceLocation {
    filename: string,
    start: RegExp
}

interface Replacement {
    source: SourceLocation,
    target: SourceLocation
}

export { Replacement }

for(let i = 0; i < config.length; i++) {
    let replacement = config[i]
    let source = fs.readFileSync(replacement.source.filename, 'utf8')
    let match = source.match(replacement.source.start)

    if(match && match.index) {
        let copy = getMatch(match, source)
        let copyIndentation = getIndentation(match.index - 1, source)
        let target = fs.readFileSync(replacement.target.filename, 'utf8')
        match = target.match(replacement.target.start)

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
            fs.writeFileSync(replacement.target.filename, newSource, 'utf8');
        }  else {
            console.log(`failed to find paste "${replacement.target.start} in "${replacement.target.filename}"`)
        }
    } else {
        console.log(`failed to find copy "${replacement.source.start} in "${replacement.source.filename}"`)
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