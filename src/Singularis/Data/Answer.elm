module Singularis.Data.Answer exposing (Answer, AnswerImage, AnswerType(..), arkanaToAnswer, desc, fromQuestion, getAnswerImage, intToArkana, list)


type alias Answer =
    { nr : Int, flipped : Bool }


type alias AnswerImage =
    ( Maybe { cross : Bool, answerType : AnswerType }, Bool )


{-| description made using synonyms from <https://www.wordhippo.com>
-}
desc : { nr : Int, flipped : Bool } -> { name : String, desc : String }
desc { nr, flipped } =
    let
        title : { name : String, desc : String }
        title =
            if flipped then
                case nr + 1 of
                    1 ->
                        { name = "Comfort"
                        , desc = "contentment, ease, relief, happiness, compassion, empathy, help, aid, relief, distress, convenience, benefit, inessential"
                        }

                    2 ->
                        { name = "Rationality"
                        , desc = "coherence, clear-headedness, purpose, sanity, focus, thought, responsibility, validity, ideology, reason, mind, charity"
                        }

                    3 ->
                        { name = "Patience"
                        , desc = "tolerance, sufferance, diligence, application, kindness, affection, control, humility, calmness, sensitivity"
                        }

                    4 ->
                        { name = "Imagination"
                        , desc = "creativity, invention, originality, attention, curiosity, fascination, idea, inspiration, initiative, vision, innovation"
                        }

                    5 ->
                        { name = "Endurance"
                        , desc = "continuation, durability, sufferance, tolerance, fortitude, resolution, maintenance, assurance, balance, hope, faith, charity"
                        }

                    6 ->
                        { name = "Separation"
                        , desc = "rift, division, disconnection, disconnect, difference, isolation, distance, conflict, termination, banishment, agreement"
                        }

                    7 ->
                        { name = "Adventurous"
                        , desc = "bold, reckless, dangerous, risky, rash, hasty, impulsive, resourceful, ambitious, eager, romantic, idealist, ruthless"
                        }

                    8 ->
                        { name = "Control"
                        , desc = "authority, management, mastery, balance, skillfulness, calmness, influence, care, protection, discipline, restriction"
                        }

                    9 ->
                        { name = "Indulgent"
                        , desc = "kind, tolerant, understanding, decadent, luxurious, relaxed, carefree, easy-going, flexible, loving, passionate, shameless"
                        }

                    10 ->
                        { name = "Opportunity"
                        , desc = "prospect, chance, circumstance, belief, hope, faith, chance, appointment, freedom, authority, choice, possibility, capability"
                        }

                    11 ->
                        { name = "Agility"
                        , desc = "liveliness, nimbleness, cleverness, mastery, capability, mobility, charm, gracefulness, elegance, alertness, cautiousness"
                        }

                    12 ->
                        { name = "Protection"
                        , desc = "security, safety, strength, care, supervision, guard, preservation, support, love, alliance, health, well-being, freedom"
                        }

                    13 ->
                        { name = "Refinement"
                        , desc = "purification, filtering, grace, polish, politeness, improvement, advancement, enhancement, rise, subtlety, change, adjustment"
                        }

                    14 ->
                        { name = "Stamina"
                        , desc = "endurance, energy, resistance, strength, force, activity, determination, strength of character, morality, drive, backbone"
                        }

                    15 ->
                        { name = "Guidance"
                        , desc = "advice, direction, help, support, assistance, instruction, information, leadership, control, supervision, protection, trust"
                        }

                    16 ->
                        { name = "Surprise"
                        , desc = "amazement, confusion, suddenness, fortune, sensation, ambush, development, revelation, disturbance, approval, puzzlement"
                        }

                    17 ->
                        { name = "Trust"
                        , desc = "confidence, faith, belief, responsibility, influence, authority, protection, commitment, optimism, investment, domination"
                        }

                    18 ->
                        { name = "Illusion"
                        , desc = "delusion, misconception, dream, misbelief, appearance, impression, mistake, misinterpretation, bluffing, falsehood, desire"
                        }

                    19 ->
                        { name = "Devotion"
                        , desc = "commitment, love, loyalty, faithfulness, spirituality, kindness, addiction, assistance, support, alliance, focus, motivation"
                        }

                    20 ->
                        { name = "Focus"
                        , desc = "attention, importance, intention, emphasis, sharpness, purpose, innocent, awareness, observation, recognition, attraction"
                        }

                    21 ->
                        { name = "Goal"
                        , desc = "intention, ambition, initiative, motivation, purpose, focus, occasion, optimism, possibility, achievement, destination"
                        }

                    _ ->
                        { name = "Error"
                        , desc = ""
                        }

            else
                case nr + 1 of
                    1 ->
                        { name = "Will"
                        , desc = "determination, drive, resolution, desire, decision, intention, desire, preference, agreeableness, choice, consciousness"
                        }

                    2 ->
                        { name = "Passion"
                        , desc = "animation, determination, dedication, devotion, drive, ambition, enthusiasm, distraction, compulsion, desire, enchantment"
                        }

                    3 ->
                        { name = "Action"
                        , desc = "execution, performance, movement, accomplishment, activity, work, effort, impact, consequence, aftermath, excitement, energy"
                        }

                    4 ->
                        { name = "Realisation"
                        , desc = "awareness, understanding, consciousness, recognition, accomplishment, achievement, awakening, manifestation, discovery"
                        }

                    5 ->
                        { name = "Inspiration"
                        , desc = "motivation, revelation, creativeness, genius, cleverness, breakthrough, imagination, influence, understanding, appreciation"
                        }

                    6 ->
                        { name = "Love"
                        , desc = "affection, devotion, friendship, attachment, respect, fascination, obsession, preference, commitment, compassion, romance"
                        }

                    7 ->
                        { name = "Victory"
                        , desc = "success, accomplishment, mastery, mastery, power, control, comfort, fortune, opportunity, comeback, domination, defeating"
                        }

                    8 ->
                        { name = "Balance"
                        , desc = "stability, harmony, difference, counterbalance, counteraction, neutrality, calmness, progression, mastery, compromise"
                        }

                    9 ->
                        { name = "Wisdom"
                        , desc = "intelligence, insight, understanding, enlightenment, benefit, rationality, traditions, suitability, usefulness, sensibility"
                        }

                    10 ->
                        { name = "Fortune"
                        , desc = "wealth, circumstance, destiny, accident, luck, future, success, creativity, capability, ambition, prediction, accomplishment"
                        }

                    11 ->
                        { name = "Strength"
                        , desc = "sturdiness, power, toughness, energy, effectiveness, determination, persistence, robustness, influence, control, force"
                        }

                    12 ->
                        { name = "Sacrifice"
                        , desc = "offer, contribute, commit, quit, rejection, consequence, accident, defeat, clearance, approach, peace offering, expense"
                        }

                    13 ->
                        { name = "Transformation"
                        , desc = "evolution, change, progress, development, compromise, settlement, adjustment, polarity, reordering, renewal, regeneration"
                        }

                    14 ->
                        { name = "Initiative"
                        , desc = "ambition, energy, ambition, ability, improvement, commitment, determination, understanding, purpose, control, creativity"
                        }

                    15 ->
                        { name = "Fate"
                        , desc = "destiny, fortune, development, mortality, chances, future, consequences, purpose, conclusion, luck, outcome, principles"
                        }

                    16 ->
                        { name = "Decision"
                        , desc = "choice, option, commitment, conclusion, determination, purposefulness, outcome, intention, exploration, agreement, deal"
                        }

                    17 ->
                        { name = "Hope"
                        , desc = "belief, desire, faith, ambition, intention, determination, understanding, cheerfulness, potential, confidence, morality"
                        }

                    18 ->
                        { name = "Insight"
                        , desc = "understanding, awareness, vision, knowledge, intelligence, observation, sharpness, principles, inspiration"
                        }

                    19 ->
                        { name = "Happiness"
                        , desc = "pleasure, satisfaction, ecstasy, optimism, suitability, health, help, opportunity, profit, occasion, progress, success"
                        }

                    20 ->
                        { name = "Restart"
                        , desc = "continue, regenerate, reestablish, restore, increase, improve, regeneration, renewal, embrace, accept, initiate, prepare"
                        }

                    21 ->
                        { name = "Reward"
                        , desc = "compensation, compensation, motivation, distinction, improvement, kindness, contribution, fortune, wealth, comfort, luck"
                        }

                    _ ->
                        { name = "Error"
                        , desc = ""
                        }
    in
    { name =
        "#"
            ++ (String.fromInt <| nr + 1)
            ++ (if flipped then
                    "*"

                else
                    ""
               )
            ++ " "
            ++ title.name
    , desc = title.desc
    }


fromQuestion : Int -> String -> Answer
fromQuestion offset =
    String.toList
        >> List.map Char.toCode
        >> List.sum
        >> (*) offset
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
