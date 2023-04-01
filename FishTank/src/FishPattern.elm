module FishPattern exposing (..)

import WaveFunCollapse exposing (Rule)


horizontal : List (Rule Bool)
horizontal =
    [ { neighbors =
            [ ( ( -1, 0 ), True )
            , ( ( 1, 0 ), True )
            , ( ( 0, -1 ), False )
            , ( ( 0, 1 ), False )
            ]
      , center = True
      }
    , { neighbors =
            [ ( ( -1, 0 ), False )
            , ( ( 1, 0 ), False )
            , ( ( 0, -1 ), True )
            , ( ( 0, 1 ), True )
            ]
      , center = False
      }
    ]


vertical : List (Rule Bool)
vertical =
    [ { neighbors =
            [ ( ( -1, 0 ), True )
            , ( ( 1, 0 ), True )
            , ( ( 0, -1 ), False )
            , ( ( 0, 1 ), False )
            ]
      , center = False
      }
    , { neighbors =
            [ ( ( -1, 0 ), False )
            , ( ( 1, 0 ), False )
            , ( ( 0, -1 ), True )
            , ( ( 0, 1 ), True )
            ]
      , center = True
      }
    ]


diagonal1 : List (Rule Bool)
diagonal1 =
    [ { neighbors =
            [ ( ( -1, 0 ), True )
            , ( ( 1, 0 ), False )
            , ( ( 0, -1 ), True )
            , ( ( 0, 1 ), False )
            ]
      , center = True
      }
    , { neighbors =
            [ ( ( -1, 0 ), False )
            , ( ( 1, 0 ), True )
            , ( ( 0, -1 ), False )
            , ( ( 0, 1 ), True )
            ]
      , center = False
      }
    ]


diagonal2 : List (Rule Bool)
diagonal2 =
    [ { neighbors =
            [ ( ( -1, 0 ), True )
            , ( ( 1, 0 ), False )
            , ( ( 0, -1 ), False )
            , ( ( 0, 1 ), True )
            ]
      , center = True
      }
    , { neighbors =
            [ ( ( -1, 0 ), False )
            , ( ( 1, 0 ), True )
            , ( ( 0, -1 ), True )
            , ( ( 0, 1 ), False )
            ]
      , center = False
      }
    ]
