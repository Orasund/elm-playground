module Singularis.Data.Answer exposing (desc, Answer, getAnswerImage,AnswerImage, AnswerType(..), list,fromQuestion,intToArkana,arkanaToAnswer)


type alias Answer =
    { nr : Int, flipped : Bool }

type alias AnswerImage =
    ( Maybe { cross : Bool, answerType : AnswerType }, Bool )

{-|
description made using synonyms from https://www.wordhippo.com
-}
desc : {nr: Int , flipped : Bool } -> {name:String,desc:String}
desc {nr,flipped} =
    let
        title : {name:String,desc:String}
        title =
            if flipped then
                case nr+1 of
                    1 ->
                        {name="Comfort"
                        ,desc="Contentment, ease, relief, happiness, compassion, empathy, help, aid, relief, distress, convenience, benefit, inessential"
                        }
                    2 -> 
                        {name="Rationality"
                        ,desc="Coherence, clear-headedness, purpose, sanity, focus, thought, responsibility, validity, ideology, reason, mind, charity"
                        }
                    3 ->
                        {name="Patience"
                        ,desc="Tolerance, sufferance, diligence, application, kindness, affection, control, calmness, humility, calmness, sensitivity"
                        }
                    4 -> 
                        {name="Imagination"
                        ,desc="Creativity, invention, originality, attention, curiosity, fascination, idea, inspiration, initiative, vision, innovation"
                        }
                    5 ->
                        {name="Endurance"
                        ,desc="Continuation, durability, sufferance, tolerance, fortitude, resolution, maintenance, assurance, balance, hope, faith, charity"
                        }
                    6 ->
                        {name="Separation"
                        ,desc="Rift, division, disconnection, disconnect, difference, isolation, distance, conflict, termination, banishment, agreement"
                        }
                    7 ->
                        {name="Adventurous"
                        ,desc="Bold, reckless, dangerous, risky, rash, hasty, impulsive, resourceful, ambitious, eager, romantic, idealist, ruthless"
                        }
                    8 ->
                        {name="Control"
                        ,desc="Authority, management, mastery, balance, skillfulness, calmness, influence, care, protection, discipline, restriction"
                        }
                    9 ->
                        {name="Indulgent"
                        ,desc="Kind, tolerant, understanding, decadent, luxurious, relaxed, carefree, easy-going, flexible, loving, passionate, shameless"
                        }
                    10 ->
                        {name="Opportunity"
                        ,desc="Prospect, chance, circumstance, belief, hope, faith, chance, appointment, freedom, authority, choice, possibility, capability"
                        }
                    11 ->
                        {name="Agility"
                        ,desc="Liveliness, nimbleness, cleverness, mastery, capability, mobility, charm, gracefulness, elegance, alertness, cautiousness"
                        }
                    12 ->
                        {name="Protection"
                        ,desc="Security, safety, strength, care, supervision, guard, preservation, support, love, alliance, health, well-being, freedom"
                        }
                    13 -> 
                        {name="Refinement"
                        ,desc="Purification, filtering, grace, polish, politeness, improvement, advancement, enhancement, rise, subtlety, change, adjustment"
                        }
                    14 ->
                        {name="Stamina"
                        ,desc="Endurance, energy, resistance, strength, force, activity, determination, strength of character, morality, drive, backbone"
                        }
                    15 ->
                        {name="Guidance"
                        ,desc="Advice, direction, help, support, assistance, instruction, information, leadership, control, supervision, protection, trust"
                        }
                    16 ->
                        {name="Surprise"
                        ,desc="Amazement, confusion, suddenness, fortune, sensation, ambush, development, revelation, disturbance, approval, puzzlement"
                        }
                    17 ->
                        {name="Trust"
                        ,desc="Confidence, faith, belief, responsibility, influence, authority, protection, commitment, optimism, investment, domination"
                        }
                    18 ->
                        {name="Illusion"
                        ,desc="Delusion, misconception, dream, misbelief, appearance, impression, mistake, misinterpretation, bluffing, falsehood, desire"
                        }
                    19 ->
                        {name="Devotion"
                        ,desc="Commitment, love, loyalty, faithfulness, spirituality, kindness, addiction, assistance, support, alliance, focus, motivation"
                        }
                    20 ->
                        {name="Focus"
                        ,desc="Attention, importance, intention, emphasis, sharpness, purpose, innocent, awareness, observation, recognition, attraction"
                        }
                    21 ->
                        {name="Goal"
                        ,desc="Intention, ambition, initiative, motivation, purpose, focus, occasion, optimism, possibility, achievement, destination"
                        }
                    _ ->
                        {name="Error"
                        ,desc=""
                        }
            else
                case nr+1 of
                    1 -> 
                        {name="Will"
                        ,desc="Determination, drive, resolution, desire, decision, intention, desire, preference, agreeableness, choice, consciousness"
                        }
                    2 -> 
                        {name="Passion"
                        ,desc="Animation, determination, dedication, devotion, drive, ambition, enthusiasm, distraction, compulsion, desire, enchantment"
                        }
                    3 -> 
                        {name="Action"
                        ,desc="Execution, performance, movement, accomplishment, activity, work, effort, impact, consequence, aftermath, excitement, energy"
                        }
                    4 -> 
                        {name="Realisation"
                        ,desc="Awareness, understanding, consciousness, recognition, accomplishment, achievement, awakening, manifestation, discovery"
                        }
                    5 -> 
                        {name="Inspiration"
                        ,desc="Motivation, revelation, creativeness, genius, cleverness, breakthrough, imagination, influence, understanding, appreciation"
                        }
                    6 -> 
                        {name="Love"
                        ,desc="Affection, devotion, friendship, attachment, respect, fascination, obsession, preference, commitment, compassion, romance"
                        }
                    7 -> 
                        {name="Victory"
                        ,desc="Success, accomplishment, mastery, mastery, power, control, comfort, fortune, opportunity, comeback, domination, defeating"
                        }
                    8 -> 
                        {name="Balance"
                        ,desc="Stability, harmony, difference, counterbalance, counteraction, neutrality, calmness, progression, mastery, compromise"
                        }
                    9 -> 
                        {name="Wisdom"
                        ,desc="Intelligence, insight, understanding, enlightenment, benefit, rationality, traditions, suitability, usefulness, sensibility"
                        }
                    10 -> 
                        {name="Fortune"
                        ,desc="Wealth, circumstance, destiny, accident, luck, future, success, creativity, capability, ambition, prediction, accomplishment"
                        }
                    11 -> 
                        {name="Strength"
                        ,desc="Sturdiness, power, toughness, energy, effectiveness, determination, persistence, robustness, influence, control, force"
                        }
                    12 -> 
                        {name="Sacrifice"
                        ,desc="Offer, contribute, commit, quit, rejection, consequence, accident, defeat, clearance, approach, peace offering, expense"
                        }
                    13 -> 
                        {name="Transformation"
                        ,desc="Evolution, change, progress, development, compromise, settlement, adjustment, polarity, reordering, renewal, regeneration"
                        }
                    14 -> 
                        {name="Initiative"
                        ,desc="Ambition, energy, ambition, ability, improvement, commitment, determination, understanding, purpose, control, creativity"
                        }
                    15 -> 
                        {name="Fate"
                        ,desc="Destiny, fortune, development, mortality, chances, future, consequences, purpose, conclusion, luck, outcome, principles"
                        }
                    16 -> 
                        {name="Decision"
                        ,desc="Choice, option, commitment, conclusion, determination, purposefulness, outcome, intention, exploration, agreement, deal"
                        }
                    17 -> 
                        {name="Hope"
                        ,desc="Belief, desire, faith, ambition, intention, determination, understanding, cheerfulness, potential, confidence, morality"
                        }
                    18 -> 
                        {name="Insight"
                        ,desc="Understanding, awareness, vision, knowledge, intelligence, observation, sharpness, principles, inspiration"
                        }
                    19 -> 
                        {name="Happiness"
                        ,desc="Pleasure, satisfaction, ecstasy, optimism, suitability, health, help, opportunity, profit, occasion, progress, success"
                        }
                    20 -> 
                        {name="Restart"
                        ,desc="Continue, regenerate, reestablish, restore, increase, improve, regeneration, renewal, embrace, accept, initiate, prepare"
                        }
                    21 -> 
                        {name="Reward"
                        ,desc="Compensation, compensation, motivation, distinction, improvement, kindness, contribution, fortune, wealth, comfort, luck"
                        }
                    _ -> 
                        {name="Error"
                        ,desc=""
                        }
    in 
    {name="#"++(String.fromInt<| nr+1) ++ (if flipped then "*" else "")++" "
        ++title.name
    ,desc=title.desc
    }

fromQuestion : String -> Answer
fromQuestion =
    String.toList
        >> List.map Char.toCode
        >> List.sum
        >> modBy 42
        >> intToArkana

intToArkana : Int -> Answer
intToArkana nr =
    { nr = nr |> modBy 21, flipped = nr > 20 }

list : List AnswerImage
list =
    List.range 0 41
        |> List.map (intToArkana >> arkanaToAnswer)


type AnswerType
    = Empty
    | Vertical
    | VerticalDouble
    | Single
    | SingleAndVerticalTop
    | SingleAndVerticalBottom
    | SingleAndVerticalBoth
    | Double
    | DoubleAndVertical
    | DoubleAndVerticalBoth



answerToArkana : AnswerImage -> Answer
answerToArkana ( maybeA, line ) =
    { nr =
        case maybeA of
            Just { cross, answerType } ->
                answerType
                    |> answerTypeToInt
                    |> (+) 1
                    |> (+)
                        (if cross then
                            10

                         else
                            0
                        )

            Nothing ->
                0
    , flipped =
        line
    }

getAnswerImage : Answer -> AnswerImage
getAnswerImage =
    arkanaToAnswer

arkanaToAnswer : Answer -> AnswerImage
arkanaToAnswer { nr, flipped } =
    if nr == 0 then
        ( Nothing
        , flipped
        )

    else
        ( Just
            { cross = nr > 10
            , answerType =
                (nr - 1)
                    |> modBy 10
                    |> intToAnswerType
            }
        , flipped
        )


intToAnswerType : Int -> AnswerType
intToAnswerType int =
    case int of
        0 ->
            Empty

        1 ->
            Vertical

        2 ->
            VerticalDouble

        3 ->
            Single

        4 ->
            SingleAndVerticalTop

        5 ->
            SingleAndVerticalBottom

        6 ->
            SingleAndVerticalBoth

        7 ->
            Double

        8 ->
            DoubleAndVertical

        9 ->
            DoubleAndVerticalBoth

        _ ->
            intToAnswerType 0


answerTypeToInt : AnswerType -> Int
answerTypeToInt t =
    case t of
        Empty ->
            0

        Vertical ->
            1

        VerticalDouble ->
            2

        Single ->
            3

        SingleAndVerticalTop ->
            4

        SingleAndVerticalBottom ->
            5

        SingleAndVerticalBoth ->
            6

        Double ->
            7

        DoubleAndVertical ->
            8

        DoubleAndVerticalBoth ->
            9
