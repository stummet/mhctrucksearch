module Model exposing (..)

--import RemoteData exposing (WebData)
import Array exposing(..)

type alias Truck =
    { 
          id                : String
        , name              : String
        , stockNumber       : Int
        , appraisalNumber   : Int
        , poNumber          : String   
        , price             : Int
        , title             : String
        , condition         : String
        , make              : String
        , model             : String
        , engineMake        : String
        , engineModel       : String
        , engineHP          : Int
        , apu               : String
        , cdl               : String
        , year              : String
        , primaryImageLink  : String
        , truckType         : String
        , salesStatus       : String
        , sleeperRoof       : String
        , sleeperBunk       : String
        , sleeperInches     : String
        , chassisNumber     : String
        , transType         : String
        , mileage           : Int
        , location          : String
        , locationName      : String
        , bodyType          : String
        , suspension        : String
        , transmission      : String
        , rearAxleType      : String
    }

type alias Model =
    { 
        -- trucks : WebData ( Array Truck )
        -- ,
        truckList : List Truck
        ,filteredTruckList : List Truck
        ,pagedTruckList : List Truck
        ,currentPageNumber : Int
    }

type alias UIModel =
    {   
        filterCDLNoSelected : Bool
        ,filterCDLYesSelected : Bool
        ,searchString : String
        ,onLoadSearchFilters : List String
        ,yearFilters : Array SearchFilterType
        ,makeFilters : Array SearchFilterType
        ,modelFilters : Array SearchFilterType
        ,salesStatusFilters : Array SearchFilterType
        ,sleeperRoofFilters : Array SearchFilterType
        ,sleeperBunkFilters : Array SearchFilterType
        ,priceFilters : Array SearchFilterType
        ,bodyTypeFilters : Array SearchFilterType
        ,suspensionFilters : Array SearchFilterType
        ,engineMakeFilters : Array SearchFilterType
        ,transmissionFilters : Array SearchFilterType
        ,rearAxleTypeFilters : Array SearchFilterType
        --,priceFilters : Array SearchFilterRangeType
        ,expandCollapseSearchFilterStates : Array SearchFilterState
        --,expandCollapseSearchFilterRangeStates : Array SearchFilterRangeState
        ,expandCollapseAllChecked : Bool
        ,showDropdown : Bool
        ,currentSortBy : SortBy
    }

type alias SearchFilterState =
    {
        index : Int
        ,searchFilterCustomType : SearchFilterCustomType
        ,userAction : Bool
    }
    
-- type alias SearchFilterRangeState =
--     {
--         index : Int
--         ,searchFilterRangeUnionType : SearchFilterRangeUnionType
--         ,userAction : Bool
--     }

type SearchFilterCustomType
    = SalesStatus
    | Year
    | Make
    | MakeModel
    | SleeperRoof
    | SleeperBunk
    | Price
    | BodyType
    | Suspension
    | EngineMake
    | Transmission
    | RearAxleType

-- type SearchFilterRangeUnionType
--     = Price

-- type SearchFilter
--     = SearchFilter SearchFilterType SearchFilterTypeVariants


-- type SearchFilterTypeVariants
--     = FloatBasedRange Float Float

-- type alias SearchFilterType =
--     {   
--         index : Int
--         ,searchFilterKey : String
--         ,userAction : Bool
--         ,resultCount : Int
--         ,filterCategory : SearchFilterCustomType
--     }

type SortBy
    = PriceLowToHigh
    | PriceHighToLow
    | MileageLowToHigh
    | MileageHighToLow
    | MakeAtoZ
    | MakeZtoA
    | YearNewToOld
    | YearOldToNew


type alias SearchFilterType =
    {   
        index : Int
        ,searchFilterKey : String
        ,searchFilterExtraData : String
        ,userAction : Bool
        ,resultCount : Int
        ,filterCategory : SearchFilterCustomType
    }

-- type alias SearchFilterRangeType =
--     {   
--         index : Int
--         ,searchFilterKey : String
--         ,searchFilterExtraData : String
--         -- ,searchFilterMinValue : Int
--         -- ,searchFilterMaxValue : Int
--         ,userAction : Bool
--         ,resultCount : Int
--         ,filterCategory : SearchFilterRangeUnionType
--     }

type alias FilterSelectionsModel =
    {   
        filterCDLNoSelected : Bool
        ,filterCDLYesSelected : Bool
    }

initialModel : Model
initialModel =
    { 
        -- trucks = RemoteData.Loading
        -- ,
        truckList = [] -- Array.empty
        ,filteredTruckList = []
        ,pagedTruckList = []
        ,currentPageNumber = 1
    }

initalUIModel : String -> UIModel
initalUIModel jsFlag =
    {
        filterCDLNoSelected = False,
        filterCDLYesSelected = False,
        searchString = "",
        onLoadSearchFilters  = String.split "&" jsFlag,
        yearFilters = Array.empty,
        makeFilters = Array.empty,
        modelFilters = Array.empty,
        salesStatusFilters = Array.empty,
        sleeperRoofFilters = Array.empty,
        sleeperBunkFilters = Array.empty,
        priceFilters = Array.empty,
        bodyTypeFilters = Array.empty,
        suspensionFilters = Array.empty,
        engineMakeFilters = Array.empty,
        transmissionFilters = Array.empty,
        rearAxleTypeFilters = Array.empty,
                                            -- this is to initialize an Array, repeat creates one item in this case and that lets us push rest of the items
                                            -- this list can be generated off of datasource, when that happens we dont need to hardcode index value, just use indexedMap
                                            -- and set the generated index value to index prop
        expandCollapseSearchFilterStates = Array.repeat 1 {index = 0,searchFilterCustomType = SalesStatus, userAction = False} 
                                                |> Array.push {index = 1,searchFilterCustomType = Year, userAction = True}
                                                |> Array.push {index = 2,searchFilterCustomType = Make, userAction = False}
                                                |> Array.push {index = 3,searchFilterCustomType = MakeModel, userAction = False}
                                                |> Array.push {index = 4,searchFilterCustomType = SleeperRoof, userAction = False}
                                                |> Array.push {index = 5,searchFilterCustomType = SleeperBunk, userAction = False}
                                                |> Array.push {index = 6,searchFilterCustomType = Price, userAction = False}
                                                |> Array.push {index = 7,searchFilterCustomType = BodyType, userAction = False}                                                
                                                |> Array.push {index = 8,searchFilterCustomType = Suspension, userAction = False}
                                                |> Array.push {index = 9,searchFilterCustomType = EngineMake, userAction = True}
                                                |> Array.push {index = 10,searchFilterCustomType = Transmission, userAction = True}
                                                |> Array.push {index = 11,searchFilterCustomType = RearAxleType, userAction = True},

        --expandCollapseSearchFilterRangeStates = Array.repeat 1 {index = 0,searchFilterRangeUnionType = Price, userAction = True},

        expandCollapseAllChecked = False,
        showDropdown = False,
        currentSortBy = MakeAtoZ
    }