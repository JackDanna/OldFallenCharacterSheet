module Neg1To4Stat

open FallenLib.Neg1To4

type Model = Neg1To4

type Msg =
    | SetNegOne
    | SetZero
    | SetOne
    | SetTwo
    | SetThree
    | SetFour

let init() : Model = Zero

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetNegOne -> NegOne
    | SetZero -> Zero
    | SetOne -> One
    | SetTwo -> Two
    | SetThree -> Three
    | SetFour -> Four

open Feliz
open Feliz.Bulma