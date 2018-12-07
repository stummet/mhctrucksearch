module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Commands exposing (..)
--import RemoteData  exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..) 
import Element.Border as Border exposing (..) 
import Element.Font as Font exposing (..) 
import TruckViews.Truck exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)
import Task
import Array exposing(..)
import String exposing (..)
import Html.Events.Extra as ExtraHtmlEvents
import SearchFilterViews.SearchFilter exposing (..)
import Element.Lazy as Lazy exposing(..)

---- INIT ----

type alias OnLoadSearchFilter =
    String

init : OnLoadSearchFilter -> ( (Model, UIModel) , Cmd Msg)
init jsflg =
    ( (initialModel,initalUIModel jsflg)
        , fetchTrucks
    )

---- UPDATE ----

performFinalSearch : Model -> String -> Model
performFinalSearch model userSearchString =
    let
        searchFilterValueList = split "=" userSearchString -- "a:THERMOKING", "md:t880", "y:2019" etc...
        searchFilterTypeCode =     
            case List.head <| searchFilterValueList of -- gives first element in the list
                Just val -> val
                Nothing -> ""
        searchFilterValue =
                -- case List.foldl (Just >> always) Nothing searchFilterValueList of  -- gives last element in the list -- 1st style
                --     Just val -> val
                --     Nothing -> ""
                case List.head <| List.reverse searchFilterValueList of -- gives last element in the list -- 2nd style
                    Just val -> val
                    Nothing -> ""

        --logsToBrowswerDevTools = Debug.log "searchValues -> " [searchFilterTypeCode,searchFilterValue]

        searchResultTruckList  = 
                if String.isEmpty userSearchString then
                    model.truckList
                else if ( (not <| String.isEmpty searchFilterTypeCode) && String.isEmpty searchFilterValue) then
                    []
                else
                    model.truckList      
                        |> List.filter (\t ->     

                                case searchFilterTypeCode of
                                    "ss"    -> startsWith  (toUpper searchFilterValue) (toUpper t.salesStatus) 
                                    "y"     -> startsWith  ( searchFilterValue) ( t.year) 
                                    "m"     -> startsWith  (toUpper searchFilterValue) (toUpper t.make) 
                                    "md"     -> startsWith  (toUpper searchFilterValue) (toUpper t.model) 
                                    "sr"     -> startsWith  (toUpper searchFilterValue) (toUpper t.sleeperRoof) 
                                    "sb"     -> startsWith  (toUpper searchFilterValue) (toUpper t.sleeperBunk) 
                                    _       -> False -- invalid search string entered by the user
                            -- else
                            --     False
                        )
                    |> List.sortBy .make
        
        finalSearchResultTruckList =
            if List.length searchResultTruckList > 0 then
                searchResultTruckList
            else
                []
        
        newModel = {model | filteredTruckList = finalSearchResultTruckList}
    in
        newModel

update : Msg -> (Model, UIModel) -> ( (Model, UIModel) , Cmd Msg  )
update msg (model, uiModel) =
    case msg of
        OnFetchTrucks response ->
            let
                trucks = case response of
                            Ok truckList ->
                                    truckList
                            Err err ->
                                    []

                --c = Debug.log "Updated year list by held salesstatus"  [trucks]--, newUIModel1.yearFilters]

                salesStatusFilters = buildSearchFilterValueRecordList SalesStatus trucks
                yearFilters = buildSearchFilterValueRecordList Year trucks
                makeFilters = buildSearchFilterValueRecordList Make trucks
                modelFilters = buildSearchFilterValueRecordList MakeModel trucks
                sleeperRoofFilters = buildSearchFilterValueRecordList SleeperRoof trucks
                sleeperBunkFilters = buildSearchFilterValueRecordList SleeperBunk trucks
            in
                ( 
                    (
                        {   model     | truckList = trucks, filteredTruckList = trucks},
                        { 
                            uiModel   | 
                                        yearFilters = yearFilters, 
                                        makeFilters = makeFilters, 
                                        modelFilters = modelFilters, 
                                        salesStatusFilters = salesStatusFilters, 
                                        sleeperRoofFilters = sleeperRoofFilters, 
                                        sleeperBunkFilters = sleeperBunkFilters 
                        }
                    )
                    , Cmd.none
                )
        
        FilterCheckBoxClicked index searchFilterCustomType userAction ->
            let

                updateUserSelectedSearchFilter : Array SearchFilterType -> (Array SearchFilterType -> UIModel) -> UIModel -- Anonymous funcs
                updateUserSelectedSearchFilter  filterList pushModifiedFilterListBackInToUIModel =
                    filterList
                        |> Array.get index
                        |> Maybe.map (\mf -> { mf | userAction = userAction} )
                        |> Maybe.map (\mf -> Array.set index mf filterList)
                        |> Maybe.map pushModifiedFilterListBackInToUIModel
                        |> Maybe.withDefault uiModel

                newUIModel = 
                    case searchFilterCustomType of
                        SalesStatus -> 
                            
                            --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | salesStatusFilters = mfArr}) ) -- first style
                            ----------------------------------------------------------------------------------------------------------------
                            -- let
                            --     fx = uiModel.salesStatusFilters
                            --             |> updateUserSelectedSearchFilter
                                
                            -- in
                            --     fx  (\mfArr -> {uiModel | salesStatusFilters = mfArr}) -- second style is a partial applications style
                            -----------------------------------------------------------------------------------------------------------------
                            (updateUserSelectedSearchFilter <| uiModel.salesStatusFilters ) (\mfArr -> {uiModel | salesStatusFilters = mfArr}) -- 3rd style is also a partial applications style
                            -----------------------------------------------------------------------------------------------------------------
                        Year -> 
                            (uiModel.yearFilters 
                                    |> updateUserSelectedSearchFilter) 
                                                            (\mfArr -> {uiModel | yearFilters = mfArr})
                            --      |> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | yearFilters = mfArr}) )
                                
                        Make -> 
                            (uiModel.makeFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | makeFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | makeFilters = mfArr}) )

                        MakeModel -> 
                            (uiModel.modelFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | modelFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | modelFilters = mfArr}) )

                        SleeperRoof -> 
                            (uiModel.sleeperRoofFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperRoofFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | sleeperRoofFilters = mfArr}) )    

                        SleeperBunk -> 
                            (uiModel.sleeperBunkFilters |> updateUserSelectedSearchFilter) (\mfArr -> {uiModel | sleeperBunkFilters = mfArr})
                                --|> (\filters -> updateUserSelectedSearchFilter filters (\mfArr -> {uiModel | sleeperBunkFilters = mfArr}) )    

                newFilteredTruckList = applySearchFilters model newUIModel

                uiModelUpdatedWithLatestSearchFilters = rebuildSearchFiltersBasedOnCurrentSearchCriteria model newUIModel
                
            in
                --( ( {model | filteredTruckList = newFilteredTruckList } , newUIModel), sendMessage SearchPressed )
                ( ( {model | filteredTruckList = newFilteredTruckList } , uiModelUpdatedWithLatestSearchFilters), Cmd.none )

        SearchString searchString ->
            let
                newModel = 
                    if String.length searchString > 0 then
                        model
                    else
                        {model | filteredTruckList = model.truckList }

            in
                ( ( newModel , {uiModel | searchString = searchString}), Cmd.none )

        SearchPressed ->
            ( (performFinalSearch model uiModel.searchString, uiModel ), Cmd.none )
            
        HandleKeyboardEvent ->
            ( (performFinalSearch model uiModel.searchString, uiModel ), Cmd.none )
        
        -- CollapseClicked searchFilterType userAction ->
        --     let
        --         x = if searchFilterType == SalesStatus then              
        --                 ( ( model , {uiModel | expandCollapseSalesStatusChecked = userAction}), Cmd.none )
        --             else
        --                 ( ( model , {uiModel | expandCollapseYearChecked = userAction}), Cmd.none )   
        --     in
        --         x

        CollapseAllClicked userAction ->
            ( ( model , {uiModel | expandCollapseAllChecked = userAction}), Cmd.none )
 

            
             
---- VIEW ----



view : (Model, UIModel) -> Html Msg
view (model, uiModel) =
        let
            searchStringBtnStyle = 
                        if String.length uiModel.searchString > 0 then 
                            [ bc 226 63 63, fc 250 250 250] 
                        else
                            [ bc 198 201 206, fc 245 245 245]

            loaderIconElement = 
                    if List.length model.truckList > 0 then
                        none
                    else
                        image [hpx 18, bw one, wf, pdl 5, bwb 2, alignTop] {src = "loader.gif", description ="Logo" }  

            searchBtnIcon =
                    if String.length uiModel.searchString > 0 then 
                        image [hpx 32, bw one] {src = "srch_white.ico", description ="Logo" }
                    else
                        image [hpx 32, bw one] {src = "srch_grey.ico", description ="Logo" }
                        
            buildCollapseAllImage userAction =
                if userAction == True then 
                    image [hpx 32, bw one] {src = "collapse.png", description ="Logo" }
                else 
                    image [hpx 32, bw one] {src = "expand.png", description ="Logo" }

            focusStyle : Element.Option
            focusStyle =
                Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
        
            navBar =
                    row[wf, hpx 75, bc 47 48 49, fc 250 250 250, alpha  0.95]
                    [
                        image [hpx 32, bw one] {src = "https://az832863.vo.msecnd.net/~/media/images/components/pagelogos/mhclogo.png?_=-381616326&h=61", description ="Logo" }
                        ,
                        el [pdl 25] <| textValue "Truck Search - Powerfull, fluid truck search platform, get the result with less than blink of an eye !!!"
                    ] 
        in
            
                layoutWith {options = [focusStyle]}  [hf, pd 0, inFront navBar ] --  inFront navBar is to make menu bar fixed
                <|
                    row[hf,wf, pdt 76]
                    [
                        column [hf, wfmin 350, pde 0 10 10 10, spy 5] -- Search Filter Panel bc 225 225 225, 
                        [
                            row[wf, pd 0, bw 1, spaceEvenly]
                            [ 
                                Input.text [wf, hf, bw 0
                                            --,Element.htmlAttribute ( on "keydown" (Decode.map HandleKeyboardEvent  decodeKeyboardEvent) )
                                            , Element.htmlAttribute(ExtraHtmlEvents.onEnter HandleKeyboardEvent)
                                        ]
                                {
                                    onChange = SearchString
                                    ,text  = uiModel.searchString
                                    ,label = labelLeft [] none
                                    ,placeholder = Just (Input.placeholder [] (el [] <| textValue "Fluid trucks Search"))

                                }
                                -- ,Input.text [wf, hf, bw 0
                                --             --,Element.htmlAttribute ( on "keydown" (Decode.map HandleKeyboardEvent  decodeKeyboardEvent) )
                                --             ,Element.htmlAttribute(ExtraHtmlEvents.onEnter HandleKeyboardEvent)
                                --         ]
                                -- {
                                --     onChange = SearchString
                                --     ,text  = uiModel.searchString
                                --     ,label = labelLeft [] none
                                --     ,placeholder = Just (Input.placeholder [] (el [] <| textValue "Fluid trucks Search"))
                                
                                -- }
                                ,Input.button ( [ hf, wpx 50, eId "submitSrch"] ++ searchStringBtnStyle)
                                    { 
                                        onPress = Just SearchPressed --if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing
                                        ,label = searchBtnIcon
                                    }
                            ]
                            ,row[pd 5, bc 221 221 221, wf, bw 0]
                            [
                                checkbox [bw one, hf, far , bw 0] {
                                    onChange = CollapseAllClicked
                                    ,icon = buildCollapseAllImage
                                    , label = labelLeft [Element.alignRight] (el [] <| textValue <| if uiModel.expandCollapseAllChecked then "Collapse Filters" else "Expand Filters" )
                                    , checked = uiModel.expandCollapseAllChecked
                                }
                            ]
                            ,column[scrollbarY,hf, wf, spy 20, bw 0]
                            [
                                if List.length model.truckList > 0 then
                                    lazy3 buildSearchFilterValuesGroup SalesStatus model uiModel
                                else
                                    loaderIconElement
                                ,if List.length model.truckList > 0 then
                                    lazy3 buildSearchFilterValuesGroup Year model uiModel
                                else
                                    none
                                ,if List.length model.truckList > 0 then
                                    lazy3 buildSearchFilterValuesGroup Make model uiModel
                                else
                                    none    
                                , if List.length model.truckList > 0 then
                                    lazy3 buildSearchFilterValuesGroup MakeModel model uiModel
                                else
                                    none
                                , if List.length model.truckList > 0 then
                                    lazy3 buildSearchFilterValuesGroup SleeperRoof model uiModel
                                else
                                    none
                                , if List.length model.truckList > 0 then
                                    lazy3 buildSearchFilterValuesGroup SleeperBunk model uiModel
                                else
                                    none                                                        
                            ]
                        ]
                        ,column[hf, wfp 5,  bwl 0, bc 235 235 235,pde 0 0 10 15 ] -- Trucks Search Result List Panel 
                        [
                            row[hf, wf, bw 0, hpx 50]
                            [ 
                                column[pdl 0, hf][] --, bc 244 66 95
                                ,column[hf, pdl 0, spaceEvenly][
                                    el [] << textValue <| "Selected Filters... ", 
                                    el [] <| textValue <| "Total used trucks found : " ++ (String.fromInt <| (List.length model.filteredTruckList))
                                ]
                            ]
                            ,column[hf, wf, scrollbarY, bw 0, pde 10 10 10 0] [ lazy trucksView model.filteredTruckList]
                            -- ,row[hf, wf, bw 0, hpx 50, pde 10 10 10 10]
                            -- [ 
                            --     column[pdl 15, hf][] --, bc 244 66 95
                            --     ,column[hf, pdl 5, spaceEvenly][
                            --         -- el [] <| textValue <| "Page nav bar... ", 
                            --         -- el [] <| textValue <| "Total Used Trucks : " ++ (String.fromInt <| (List.length model.truckList))
                            --     ]
                            -- ]                  
                        ]
                        ,column[bw 2, wfp 4, hf, pd 0]
                        [
                            row[bc 200 200 200, wf, hf, scrollbarY]
                            [
                                el [alignTop] <| textValue <| "Truck Details"
                            ]
                            
                        ]
                    ]
            
---- PROGRAM ----


main : Program OnLoadSearchFilter (Model,UIModel) Msg
main =
    Browser.element
        { view = view
        , init = init -- to capture flags from JS
        , update = update
        , subscriptions = always Sub.none
        }
