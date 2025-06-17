module MockData exposing (Firm, Lawyer, firms, lawyers)

type alias Firm =
    { name : String
    , version : String
    , date : String
    , accepted : Bool
    }

type alias Lawyer =
    { id : Int
    , name : String
    , specialty : String
    }

firms : List Firm
firms =
    [ { name = "Wilson Law", version = "v1.0", date = "Jan 12, 2024", accepted = True }
    , { name = "Partners & Co", version = "v1.1", date = "Mar. 2, 2024", accepted = False }
    ]

lawyers : List Lawyer
lawyers =
    [ { id = 1, name = "John Smith", specialty = "Litigation" }
    , { id = 2, name = "Mary Jones", specialty = "Family Law" }
    ]
