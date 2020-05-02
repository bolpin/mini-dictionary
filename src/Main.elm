module Main exposing (Model, Msg(..), SortDirection(..), Term, filterTerms, initTerms, main, numPages, onePageTerms, sortDirString, update, view, viewControls, viewFooter, viewOnePage, viewPaginator, viewPaginatorControls, viewRow, viewRows, viewTableHeader)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (autofocus, colspan, disabled, href, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Random
import Random.List exposing (choose)
import String exposing (toUpper)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { terms : List Term
    , numPerPage : Int
    , page : Int
    , filter : String
    , filteredLength : Int
    , sortField : String
    , sortDirection : SortDirection
    , randomTerm : Term
    , definitionHidden : Bool
    }


type alias Term =
    { name : String
    , description : String
    }


type SortDirection
    = Ascending
    | Descending


init : () -> ( Model, Cmd Msg )
init _ =
    ( { terms = initTerms
      , numPerPage = 15
      , page = 1
      , filter = ""
      , filteredLength = List.length initTerms
      , sortField = "name"
      , sortDirection = Ascending
      , randomTerm = { name = "", description = "" }
      , definitionHidden = True
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateFilter String
    | SortBy String
    | GoToFirst
    | GoToPrev
    | GoToNext
    | GoToLast
    | GenerateRandomTerm
    | UpdateRandomTerm Term
    | DisplayRandomTermDefinition


randomTermGenerator : List Term -> Random.Generator Term
randomTermGenerator terms =
    Random.uniform { name = "-", description = "-" } terms


update msg model =
    case msg of
        GenerateRandomTerm ->
            ( model, Random.generate UpdateRandomTerm (randomTermGenerator model.terms) )

        UpdateRandomTerm aTerm ->
            ( { model | randomTerm = aTerm, definitionHidden = True }, Cmd.none )

        DisplayRandomTermDefinition ->
            ( { model | definitionHidden = False }, Cmd.none )

        GoToFirst ->
            ( { model | page = 1 }, Cmd.none )

        GoToPrev ->
            ( { model | page = Basics.max 1 (model.page - 1) }, Cmd.none )

        GoToNext ->
            ( { model | page = Basics.min (numPages model) (model.page + 1) }, Cmd.none )

        GoToLast ->
            ( { model | page = numPages model }, Cmd.none )

        UpdateFilter newFilter ->
            let
                remainingTermsCount =
                    List.length (filterTerms newFilter model.terms)
            in
            ( { model | page = 1, filter = newFilter, filteredLength = remainingTermsCount }, Cmd.none )

        SortBy newField ->
            let
                swapDirection =
                    case model.sortDirection of
                        Ascending ->
                            Descending

                        Descending ->
                            Ascending

                newDirection newSortField =
                    case model.sortField == newSortField of
                        True ->
                            swapDirection

                        False ->
                            model.sortDirection
            in
            ( { model
                | page = 1
                , sortField = newField
                , sortDirection = newDirection newField
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "padding" "2em"
        , style "background-color" "#EBF5FB"
        , style "font-family" "sans-serif"
        , style "min-height" "1000px"
        ]
        [ h1 [] [ text "Dictionary" ]
        , viewRandomTerm model
        , viewControls model
        , viewPaginator model
        , viewOnePage model
        ]


viewRandomTerm : Model -> Html Msg
viewRandomTerm model =
    p
        [ style "min-height" "140px"
        ]
        [ button
            [ onClick GenerateRandomTerm
            , style "padding-left" "1em"
            , style "padding-right" "1em"
            , style "display" "inline"
            , style "cursor" "pointer"
            ]
            [ text "random" ]
        , button
            [ onClick DisplayRandomTermDefinition
            , style "display" "inline"
            , style "margin-left" "2em"
            , style "padding-right" "1em"
            , style "cursor" "pointer"
            ]
            [ text "show" ]
        , viewRandomRow model
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    footer []
        [ ul []
            [ li [] [ text model.sortField ]
            , li [] [ text (String.fromInt model.page) ]
            ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    header []
        [ input
            [ type_ "text"
            , placeholder "Filter by Term"
            , autofocus True
            , value model.filter
            , onInput UpdateFilter
            , style "font-size" "1.4em"
            , style "height" "24px"
            ]
            []
        ]


viewOnePage : Model -> Html Msg
viewOnePage model =
    table []
        (viewRows model)


onePageTerms : List Term -> Int -> Int -> List Term
onePageTerms lst page numPerPage =
    let
        startIndex =
            (page - 1) * numPerPage
    in
    lst
        |> List.drop startIndex
        |> List.take numPerPage


viewRows : Model -> List (Html Msg)
viewRows model =
    let
        filteredTerms =
            filterTerms model.filter model.terms

        sortedFilteredTerms =
            case ( model.sortField, model.sortDirection ) of
                ( "name", Ascending ) ->
                    List.sortBy .name filteredTerms

                ( "name", Descending ) ->
                    List.reverse <| List.sortBy .name filteredTerms

                ( "description", Ascending ) ->
                    List.sortBy .description filteredTerms

                ( _, _ ) ->
                    List.reverse <| List.sortBy .description filteredTerms

        onePage =
            onePageTerms sortedFilteredTerms model.page model.numPerPage
    in
    viewTableHeader model.sortField model.sortDirection
        :: List.map viewRow onePage


filterTerms : String -> List Term -> List Term
filterTerms filterString terms =
    let
        nameMatches term =
            String.contains (String.toLower filterString) (String.toLower term.name)
    in
    List.filter nameMatches terms


sortDirString : String -> String -> SortDirection -> String
sortDirString sortField field sortDirection =
    case ( sortField == field, sortDirection ) of
        ( False, _ ) ->
            ""

        ( True, Ascending ) ->
            " ⬇"

        ( True, Descending ) ->
            " ⬆"


viewTableHeader : String -> SortDirection -> Html Msg
viewTableHeader sortField sortDirection =
    let
        nameHeader =
            "Term" ++ sortDirString sortField "name" sortDirection

        descriptionHeader =
            "Definition" ++ sortDirString sortField "description" sortDirection
    in
    tr []
        [ th []
            [ a
                [ href "#"
                , onClick (SortBy "name")
                ]
                [ text nameHeader ]
            ]
        , th []
            [ a
                [ href "#"
                , onClick (SortBy "description")
                ]
                [ text descriptionHeader ]
            ]
        ]


viewRandomRow : Model -> Html Msg
viewRandomRow model =
    let
        displayType =
            case model.definitionHidden of
                True ->
                    "none"

                _ ->
                    "inline"
    in
    p []
        [ text (String.toUpper model.randomTerm.name)
        , p [ style "display" displayType ]
            [ text (" - " ++ model.randomTerm.description) ]
        ]


viewRow : Term -> Html Msg
viewRow term =
    tr []
        [ td
            [ style "minWidth" "150px" ]
            [ text term.name ]
        , td []
            [ text term.description ]
        ]


viewPaginator : Model -> Html Msg
viewPaginator model =
    div []
        [ text
            ("page "
                ++ String.fromInt model.page
                ++ " of "
                ++ String.fromInt (numPages model)
            )
        , viewPaginatorControls model
        ]


numPages : Model -> Int
numPages model =
    1 + (model.filteredLength // model.numPerPage)


viewPaginatorControls : Model -> Html Msg
viewPaginatorControls model =
    let
        isFirst =
            model.page == 1

        isLast =
            model.page == numPages model
    in
    div []
        [ button
            [ onClick GoToFirst
            , disabled isFirst
            , style "padding-left" "1em"
            , style "padding-right" "1em"
            , style "cursor" "pointer"
            ]
            [ text "first" ]
        , button
            [ onClick GoToPrev
            , disabled isFirst
            , style "padding-left" "1em"
            , style "padding-right" "1em"
            , style "cursor" "pointer"
            ]
            [ text "prev" ]
        , button
            [ onClick GoToNext
            , disabled isLast
            , style "padding-left" "1em"
            , style "padding-right" "1em"
            , style "cursor" "pointer"
            ]
            [ text "next" ]
        , button
            [ onClick GoToLast
            , disabled isLast
            , style "padding-left" "1em"
            , style "padding-right" "1em"
            , style "cursor" "pointer"
            ]
            [ text "last" ]
        ]



-- DATA


initTerms : List Term
initTerms =
    [ { name = "nibh", description = "Est fugiat dolor do in ad tempor laborum esse." }
    , { name = "ad", description = "Fugiat minim Lorem minim proident exercitation ex." }
    , { name = "adipisicing", description = "Cupidatat officia eu in adipisicing consequat sunt culpa in ipsum esse incididunt incididunt." }
    , { name = "aliqua", description = "Amet ea irure quis ad sint exercitation labore minim." }
    , { name = "aliquip", description = "Aliqua incididunt nostrud ad nisi non ipsum." }
    , { name = "amet", description = "Dolore ipsum dolore Lorem pariatur ad nulla et occaecat laborum do minim exercitation laboris." }
    , { name = "anim", description = "Esse deserunt consectetur nostrud incididunt incididunt adipisicing proident consectetur ad aute eiusmod officia." }
    , { name = "arcu", description = "Mollit officia non duis consequat adipisicing." }
    , { name = "aute", description = "Veniam anim ad culpa sunt amet pariatur nostrud consectetur laboris nulla dolor do deserunt." }
    , { name = "blandit", description = "Excepteur tempor Lorem laborum nulla velit nisi sit anim ullamco commodo enim." }
    , { name = "cillum", description = "Duis commodo labore consequat consectetur est esse quis laborum laboris Lorem excepteur sint irure anim." }
    , { name = "commodo", description = "Id dolor nulla dolor minim dolore enim proident pariatur ipsum." }
    , { name = "condimentum", description = "Aliqua nisi aute in fugiat cillum quis sunt." }
    , { name = "congue", description = "Cillum amet anim officia quis est officia." }
    , { name = "consectetur", description = "Ullamco tempor voluptate do tempor." }
    , { name = "consequat", description = "Consequat nisi est ullamco sunt." }
    , { name = "culpa", description = "Qui elit incididunt laboris dolor labore culpa eu eiusmod Lorem nostrud ad ipsum est ut." }
    , { name = "cupidatat", description = "Reprehenderit nisi culpa veniam dolore ea esse minim eiusmod." }
    , { name = "curabitur", description = "Commodo deserunt adipisicing ullamco irure magna Lorem et laboris sit anim." }
    , { name = "curabitur", description = "In sit labore cupidatat dolore aliquip voluptate et nostrud magna do." }
    , { name = "cursus", description = "Incididunt laboris sint nostrud tempor laboris." }
    , { name = "deserunt", description = "Pariatur elit non est dolor." }
    , { name = "dictumst", description = "Minim laboris reprehenderit eiusmod voluptate sint exercitation do fugiat eiusmod." }
    , { name = "do", description = "Aute pariatur ex mollit mollit aliqua veniam et." }
    , { name = "dolore", description = "Duis in fugiat elit reprehenderit cillum ea dolore quis eu est esse sunt eu." }
    , { name = "duis", description = "Consectetur tempor exercitation magna consectetur proident aliqua cillum occaecat irure quis dolor." }
    , { name = "ea", description = "Nostrud officia id consequat minim do amet aliquip aute." }
    , { name = "eget", description = "Laboris non excepteur adipisicing irure deserunt id non." }
    , { name = "eiusmod", description = "Do culpa ipsum dolor in." }
    , { name = "habitant", description = "Velit duis minim laborum irure duis commodo ex enim tempor." }
    , { name = "elementum", description = "Aliquip ut nulla quis cupidatat aliquip consequat Lorem ipsum." }
    , { name = "elit", description = "Veniam in dolor labore nisi qui commodo consectetur exercitation cupidatat adipisicing." }
    , { name = "enim", description = "Cupidatat dolor ex voluptate Lorem labore consectetur nisi eu cupidatat sit consequat ipsum." }
    , { name = "esse", description = "Ullamco est ad ea labore sunt et." }
    , { name = "et", description = "Labore reprehenderit minim dolor esse fugiat labore commodo eiusmod veniam minim laborum." }
    , { name = "pellentesque", description = "Cillum dolor adipisicing labore amet sunt pariatur." }
    , { name = "ex", description = "Velit et ad pariatur nostrud Lorem pariatur sunt qui labore." }
    , { name = "exercitation", description = "Minim enim exercitation nostrud quis dolor esse nostrud cupidatat excepteur sit et in anim." }
    , { name = "fermentum", description = "Qui aliqua velit est incididunt esse irure ullamco minim Lorem fugiat deserunt voluptate adipisicing laboris." }
    , { name = "fiat", description = "Minim sint nisi minim est." }
    , { name = "fringilla", description = "In officia et non ex adipisicing ea commodo." }
    , { name = "fugiat", description = "Sit aliqua aute mollit qui do proident deserunt eu aute sint minim tempor ex." }
    , { name = "gravida", description = "Laborum cupidatat magna sunt minim commodo eiusmod culpa qui pariatur aliqua ex excepteur tempor." }
    , { name = "hoc", description = "Magna duis consectetur culpa nulla laboris anim amet ullamco officia aliquip do." }
    , { name = "id", description = "Et duis dolore nisi exercitation laboris do et anim tempor voluptate nostrud aute do aute." }
    , { name = "in", description = "Eiusmod magna irure mollit proident sint proident mollit cillum nostrud." }
    , { name = "incididunt", description = "Irure cupidatat aute mollit dolore ullamco in ullamco eu proident veniam velit elit laboris nisi." }
    , { name = "ipsum", description = "Fugiat aliqua esse aute anim dolore ex laboris do ipsum deserunt do fugiat non consequat." }
    , { name = "irure", description = "Eiusmod ullamco anim dolore adipisicing pariatur officia irure dolore irure officia eu pariatur ullamco." }
    , { name = "labore", description = "Mollit esse aute in elit mollit dolor voluptate aute sunt nostrud amet consectetur velit ullamco." }
    , { name = "laboris", description = "Enim adipisicing duis nisi tempor do do consequat Lorem quis." }
    , { name = "laborum", description = "Sunt tempor exercitation laboris ullamco eu ullamco consectetur." }
    , { name = "lacus", description = "Laborum Lorem quis excepteur culpa duis." }
    , { name = "luctus", description = "Id sit ut ullamco incididunt incididunt dolor do eiusmod culpa nulla incididunt pariatur." }
    , { name = "lux", description = "Id reprehenderit labore consequat consequat minim excepteur." }
    , { name = "malesuada", description = "Esse magna non id ea." }
    , { name = "manga", description = "Ipsum consequat consequat duis adipisicing laboris." }
    , { name = "massa", description = "Ex eiusmod duis adipisicing tempor." }
    , { name = "mauris", description = "Eiusmod tempor mollit sint nostrud enim nostrud esse reprehenderit qui." }
    , { name = "minim", description = "Consectetur id anim aliqua incididunt aute duis nulla aute." }
    , { name = "molestie", description = "Ipsum quis excepteur ipsum ullamco ad." }
    , { name = "mollit", description = "Ad voluptate consectetur amet tempor sit magna." }
    , { name = "morbi", description = "Amet ex ad officia quis tempor ad anim." }
    , { name = "nauseum", description = "Velit elit aliqua commodo officia est mollit veniam commodo est consequat non fugiat ullamco." }
    , { name = "nisi", description = "Eiusmod nisi quis duis magna sit et dolore sit nostrud aliqua irure eiusmod quis." }
    , { name = "non", description = "Consequat magna officia non non culpa irure exercitation." }
    , { name = "nulla", description = "Dolore occaecat excepteur enim culpa cupidatat nisi esse laborum laborum officia laborum." }
    , { name = "nullam", description = "Voluptate do elit laborum ut elit Lorem laboris aliqua dolore aliquip excepteur cillum mollit eiusmod." }
    , { name = "occaecat", description = "Aliquip laborum quis et magna adipisicing officia qui aute minim Lorem consectetur." }
    , { name = "odio", description = "Sit sint incididunt sint consequat amet incididunt eiusmod." }
    , { name = "officia", description = "Lorem Lorem sint duis velit labore pariatur Lorem officia ipsum fugiat irure quis labore irure." }
    , { name = "orci", description = "Nostrud minim commodo cillum laboris sit est." }
    , { name = "pariatur", description = "Cillum cupidatat enim fugiat excepteur esse amet ipsum cupidatat ipsum." }
    , { name = "pariatur", description = "Sunt et sunt commodo commodo id nostrud pariatur enim labore irure." }
    , { name = "phasellus", description = "Eiusmod esse minim consectetur nulla magna et dolor." }
    , { name = "porttitor", description = "Mollit esse dolor veniam qui amet sint nisi ut sint magna proident sit." }
    , { name = "posuere", description = "Eiusmod minim laborum Lorem magna velit sunt officia voluptte." }
    , { name = "potenti", description = "Exercitation et consectetur ad id ipsum laborum ad aliqua occaecat do officia consectetur occaecat." }
    , { name = "proident", description = "Est qui incididunt proident velit ut ut deserunt elit deserunt commodo nostrud." }
    , { name = "qui", description = "Nisi cillum pariatur labore ea labore laboris officia Lorem magna." }
    , { name = "quis", description = "Excepteur officia mollit dolor qui proident." }
    , { name = "reprehenderit", description = "Ex cillum aliqua aliqua ea proident laboris." }
    , { name = "sapien", description = "Elit magna laboris quis qui excepteur adipisicing ad aliquip." }
    , { name = "sed", description = "Do excepteur cillum aliquip duis magna qui culpa pariatur reprehenderit." }
    , { name = "semper", description = "Sunt ea esse magna dolor ipsum." }
    , { name = "sint", description = "Laborum tempor do voluptate consectetur magna consectetur voluptate ad eiusmod minim Lorem minim." }
    , { name = "sunt", description = "Ullamco do eu quis id ea sunt occaecat non sunt id magna nulla ipsum tempor." }
    , { name = "suscipit", description = "Magna mollit do reprehenderit qui cupidatat sunt aliqua nostrud esse." }
    , { name = "tempor", description = "Labore cupidatat velit incididunt commodo enim ex sunt ut nulla exercitation amet." }
    , { name = "tempor", description = "Velit nostrud ad magna exercitation est anim mollit pariatur sunt pariatur nisi." }
    , { name = "tristique", description = "Dolor deserunt cupidatat eu laborum enim in reprehenderit cupidatat nostrud." }
    , { name = "turpis", description = "Est ipsum fugiat anim id dolore do veniam eu esse ullamco." }
    , { name = "ullamco", description = "Officia duis officia officia mollit laboris ullamco labore nostrud incididunt." }
    , { name = "ultrices", description = "Do officia nulla commodo cillum sint." }
    , { name = "ut", description = "Elit mollit irure commodo ut magna officia velit occaecat ut amet sunt non." }
    , { name = "velit", description = "Velit excepteur nostrud ullamco ipsum pariatur Lorem quis in ullamco aliquip sunt velit non." }
    , { name = "veniam", description = "Reprehenderit adipisicing occaecat exercitation duis anim occaecat labore." }
    , { name = "vidi", description = "Pariatur ipsum commodo laborum laborum et." }
    , { name = "vitae", description = "Do consectetur ut dolore voluptate nisi ex." }
    , { name = "voluptate", description = "Ea reprehenderit amet aute dolor cillum duis labore ullamco consectetur dolor fugiat aliquip." }
    ]
