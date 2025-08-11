module Pages.Tutorial exposing (page)

import Element exposing (..)
import Spa.Page as Page
import Ui
import View


page : a -> Page.Page flags sharedMsg (View.View msg) () ()
page _ =
    Page.static <|
        View.View "Moongle — Tutorial"
            (column [ Ui.container, width fill, spacing 16, padding 20 ]
                [ Ui.card [] (text "Write a short tutorial here — keyboard shortcuts, query grammar, examples, etc.")
                ]
            )
