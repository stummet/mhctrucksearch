
-- module SearchFilterViews.MakeSearchFilter exposing (..)

-- import Element exposing (..)
-- import Element.Input exposing (..)
-- import Helpers.ElmStyleShotcuts exposing (..)
-- import Helpers.ElmUI exposing (..)
-- import Helpers.Utils exposing (..)
-- import Model exposing (..)
-- import Msg exposing (..)
-- import List.Unique exposing (..)
-- import Array exposing (..)
 
-- --flippedComparison a b =
-- desendingOrder a b =
--     case compare a b of
--         LT -> GT
--         EQ -> EQ
--         GT -> LT
        
-- buildMakeValueList : List Truck -> Array String
-- buildMakeValueList trucks =
--     List.map (\t -> t.make) trucks
--         |> filterDuplicates
--         |> List.sort -- to do descending order
--         |> Array.fromList

-- buildMakeValueRecordList : List Truck -> Array SearchFilterType
-- buildMakeValueRecordList trucks =
--     buildMakeValueList trucks
--         |> Array.map (\make -> {searchFilterKey = make, userAction = False, resultCount = 0})

-- buildMakeValuesGroup : Model -> UIModel -> Element Msg
-- buildMakeValuesGroup model uiModel = --currentFilteredTrucks =
--     let
--         makeFilters = uiModel.makeFilters

--         buildMakeCheckboxes :  Int -> SearchFilterType -> Element Msg
--         buildMakeCheckboxes index makeSearchFilter =
--             let
--                 makeWiseCount =    List.filter (\t -> String.trim t.make == makeSearchFilter.searchFilterKey) model.filteredTruckList --currentFilteredTrucks
--             in
--                 row[bw two]
--                 [
--                     checkbox [bw one, pdr 5 ] {
--                         onChange = FilterMakeCheckBoxClicked index
--                         ,icon = buildChkBoxImage
--                         , label = labelRight [] (el [] <| textValue makeSearchFilter.searchFilterKey )
--                         --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
--                         , checked = makeSearchFilter.userAction
--                     }
--                     , textValue <| " (" ++  (String.fromInt <| (List.length makeWiseCount))  ++ ")"
--                 ]
--                 -- if List.length yearWiseCount > 0 then
--                 --     row[bw two]
--                 --     [
--                 --         checkbox [bw one, pdr 5 ] {
--                 --             onChange = FilterYearCheckBoxClicked index year
--                 --             ,icon = buildChkBoxImage
--                 --             , label = labelRight [] (el [] <| textValue (String.fromInt year) )
--                 --             --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
--                 --             , checked = userAction
--                 --         }
--                 --         , textValue <| " (" ++  (String.fromInt <| (List.length yearWiseCount))  ++ ")"
--                 --     ]
--                 -- else
--                 --     none
--     in
--         row[spy 15, wf]
--         [
--             column[spy 10, wf,  bw one]
--             [
--                 row[bw 0, hf, bwb 1, wf, pdb 3]
--                 [
--                     paragraph [bw one, fal, wf, bc 200 200 200, hpx 25, pd 5, centerY][textValue <| "Make"]
--                 ]
--                 ,column[spy 10, pdl 15, hf, scrollbarY, wf]
--                 (
--                     Array.toList <| Array.indexedMap buildMakeCheckboxes makeFilters -- column function needs List of item and not Array of items, so need conversion
--                 )
--             ]
--         ]

-- buildChkBoxImage userAction =
--         if userAction == True then 
--             image [hpx 24] {src = "checked.png", description ="Logo" }
--         else 
--             el [hpx 24, wpx 24, bw 2, br 5] <| none