module CharacterEffectList

type Msg =
    | ModifyCharacterEffect of int * CharacterEffect.Msg
    | Insert of string
    | Remove of int