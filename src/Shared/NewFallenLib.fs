namespace NewFallenLib

module Dice =

    open FParsec

    type DicePoolPenalty =
        private
        | DicePoolPenalty of uint
        static member (+)(DicePoolPenalty a, DicePoolPenalty b) = DicePoolPenalty(a - b)

    let pDicePoolPenalty: Parser<DicePoolPenalty, unit> =
        pchar '-' >>. puint32 |>> DicePoolPenalty

    type D4 =
        private
        | D4 of uint
        static member (+)(D4 a, D4 b) = D4(a + b)
        static member (-)(D4 a, DicePoolPenalty b) = D4(a - b)
        static member uint(D4 num) = num

    type D6 =
        private
        | D6 of uint
        static member (+)(D6 a, D6 b) = D6(a + b)
        static member (-)(D6 a, DicePoolPenalty b) = D6(a - b)
        static member uint(D6 num) = num

    type D8 =
        private
        | D8 of uint
        static member (+)(D8 a, D8 b) = D8(a + b)
        static member (-)(D8 a, DicePoolPenalty b) = D8(a - b)
        static member uint(D8 num) = num

    type D10 =
        private
        | D10 of uint
        static member (+)(D10 a, D10 b) = D10(a + b)
        static member (-)(D10 a, DicePoolPenalty b) = D10(a - b)
        static member uint(D10 num) = num

    type D12 =
        private
        | D12 of uint
        static member (+)(D12 a, D12 b) = D12(a + b)
        static member (-)(D12 a, DicePoolPenalty b) = D12(a - b)
        static member uint(D12 num) = num

    type D20 =
        private
        | D20 of uint
        static member (+)(D20 a, D20 b) = D20(a + b)
        static member (-)(D20 a, DicePoolPenalty b) = D20(a - b)
        static member uint(D20 num) = num

    type Die =
        | D4Die of D4
        | D6Die of D6
        | D8Die of D8
        | D10Die of D10
        | D12Die of D12
        | D20Die of D20


    let pDie diceFaces =
        puint32 .>> pchar 'd' .>> pstring diceFaces

    let pD4: Parser<D4, unit> = pDie "4" |>> D4
    let pD6: Parser<D6, unit> = pDie "6" |>> D6
    let pD8: Parser<D8, unit> = pDie "8" |>> D8
    let pD10: Parser<D10, unit> = pDie "10" |>> D10
    let pD12: Parser<D12, unit> = pDie "12" |>> D12
    let pD20: Parser<D20, unit> = pDie "20" |>> D20

    let d4ToString d4 = $"{d4}d4"
    let d6ToString d6 = $"{d6}d6"
    let d8ToString d8 = $"{d8}d8"
    let d10ToString d10 = $"{d10}d10"
    let d12ToString d12 = $"{d12}d12"
    let d20ToString d20 = $"{d20}d20"

    type DicePool =
        { d4: D4
          d6: D6
          d8: D8
          d10: D10
          d12: D12
          d20: D20 }

    let pDicePool =
        parse {
            let! d20 = opt pD20 |>> Option.defaultValue (D20 0u)
            let! d12 = opt pD12 |>> Option.defaultValue (D12 0u)
            let! d10 = opt pD10 |>> Option.defaultValue (D10 0u)
            let! d8 = opt pD8 |>> Option.defaultValue (D8 0u)
            let! d6 = opt pD6 |>> Option.defaultValue (D6 0u)
            let! d4 = opt pD4 |>> Option.defaultValue (D4 0u)

            return
                { d4 = d4
                  d6 = d6
                  d8 = d8
                  d10 = d10
                  d12 = d12
                  d20 = d20 }
        }
