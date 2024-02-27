module Firework exposing (Firework, Msg, burst, init, isEmpty, sub, update, view)

import Html exposing (Attribute, Html)
import Html.Style
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Random exposing (Generator, Seed)
import Random.Extra
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs


type alias Firework =
    System FireworkParticle


type FireworkParticle
    = Fizzler Color


type Color
    = Red
    | Green
    | Blue


type alias Msg =
    System.Msg FireworkParticle


viewSize =
    700


init : Seed -> System FireworkParticle
init =
    System.init


isEmpty : System FireworkParticle -> Bool
isEmpty =
    System.hasParticles


fizzler : Color -> Generator (Particle FireworkParticle)
fizzler color =
    Particle.init (Random.constant (Fizzler color))
        |> Particle.withDirection (Random.map degrees (Random.float 0 360))
        |> Particle.withSpeed (Random.map (clamp 0 200) (normal 100 100))
        |> Particle.withLifetime (normal 1.25 0.1)


fireworkAt : Color -> Float -> Float -> Generator (List (Particle FireworkParticle))
fireworkAt color x y =
    fizzler color
        |> Particle.withLocation (Random.constant { x = x, y = y })
        |> Particle.withGravity 50
        |> Particle.withDrag
            (\_ ->
                { coefficient = 1
                , density = 0.015
                , area = 2
                }
            )
        |> Random.list 150


update : System.Msg FireworkParticle -> Firework -> Firework
update =
    System.update


burst : Firework -> Firework
burst =
    System.burst
        (Random.Extra.andThen3 fireworkAt
            (Random.uniform Red [ Green, Blue ])
            (normal (viewSize / 2) 50)
            (normal (viewSize / 2) 50)
        )


sub : (System.Msg FireworkParticle -> msg) -> Firework -> Sub msg
sub =
    System.sub []


view : List (Attribute msg) -> Firework -> Html msg
view attrs model =
    System.view fireworkView
        ([ Html.Style.widthPx viewSize
         , Html.Style.heightPx viewSize
         ]
            ++ attrs
        )
        model


fireworkView : Particle FireworkParticle -> Svg msg
fireworkView particle =
    case Particle.data particle of
        Fizzler color ->
            let
                length =
                    max 4 (Particle.speed particle / 15)

                ( hue, saturation, luminance ) =
                    toHsl color

                maxLuminance =
                    100

                luminanceDelta =
                    maxLuminance - luminance

                lifetime =
                    Particle.lifetimePercent particle

                opacity =
                    if lifetime < 0.1 then
                        lifetime * 10

                    else
                        1
            in
            Svg.ellipse
                [ -- location within the burst
                  SAttrs.cx (String.fromFloat (length / 2))
                , SAttrs.cy "0"

                -- size, smeared by motion
                , SAttrs.rx (String.fromFloat length)
                , SAttrs.ry "4"
                , SAttrs.transform ("rotate(" ++ String.fromFloat (Particle.directionDegrees particle) ++ ")")

                -- color!
                , SAttrs.opacity (String.fromFloat opacity)
                , SAttrs.fill
                    (hslString
                        hue
                        saturation
                        (maxLuminance - luminanceDelta * (1 - lifetime))
                    )
                ]
                []


{-| Using the tango palette, but a little lighter. Original colors at
-}
toHsl : Color -> ( Float, Float, Float )
toHsl color =
    case color of
        Red ->
            -- scarlet red
            ( 0, 86, 75 )

        Green ->
            -- chameleon
            ( 90, 75, 75 )

        Blue ->
            -- sky blue
            ( 211, 49, 83 )


hslString : Float -> Float -> Float -> String
hslString hue saturation luminance =
    "hsl("
        ++ String.fromFloat hue
        ++ ","
        ++ String.fromFloat saturation
        ++ "%,"
        ++ String.fromFloat luminance
        ++ "%)"
