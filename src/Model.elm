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
        , price             : Float
        , title             : String
        , condition         : String
        , make              : String
        , model             : String
        , engineMake        : String
        , engineModel       : String
        , engineHP          : Float
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
        , mileage           : Float
        , location          : String
        , locationName      : String
        , salesStatusFlag   : String
        , bodyType          : String
        , suspension        : String
        , rearAxleType      : String
        , wheelBase         : Float
        , frontAxleWeight   : Float
        , rearAxleWeight    : Float
        , fleetCode         : String  
        , truckStatus       : String
        , specialFinancing  : String
        , inventoryAge      : Float
        , owningBranch      : String
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
        ,engineHPFilters : Array SearchFilterType
        --,priceFilters : Array SearchFilterRangeType
        ,engineMakeFilters : Array SearchFilterType
        ,sleeperInchesFilters : Array SearchFilterType
        ,transTypeFilters : Array SearchFilterType
        ,bodyTypeFilters : Array SearchFilterType
        ,suspensionFilters : Array SearchFilterType
        ,rearAxleTypeFilters : Array SearchFilterType
        ,wheelBaseFilters : Array SearchFilterType
        ,mileageFilters : Array SearchFilterType
        ,frontAxleWeightFilters : Array SearchFilterType
        ,rearAxleWeightFilters : Array SearchFilterType
        ,fleetCodeFilters : Array SearchFilterType
        ,truckStatusFilters : Array SearchFilterType
        ,specialFinancingFilters : Array SearchFilterType
        ,inventoryAgeFilters : Array SearchFilterType        
        ,owningBranchFilters : Array SearchFilterType
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
    | EngineHP
    | EngineMake    
    | SleeperInches
    | TransType
    | BodyType
    | Suspension
    | RearAxleType
    | WheelBase
    | Mileage
    | FrontAxleWeight
    | RearAxleWeight
    | FleetCode
    | TruckStatus
    | SpecialFinancing
    | InventoryAge
    | OwningBranch

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
        engineHPFilters = Array.empty,
        engineMakeFilters = Array.empty,
        sleeperInchesFilters = Array.empty,
        transTypeFilters = Array.empty,
        bodyTypeFilters = Array.empty,
        suspensionFilters = Array.empty,
        rearAxleTypeFilters = Array.empty,
        wheelBaseFilters = Array.empty,
        mileageFilters = Array.empty,
        frontAxleWeightFilters = Array.empty,
        rearAxleWeightFilters = Array.empty,
        fleetCodeFilters = Array.empty,
        truckStatusFilters = Array.empty,
        specialFinancingFilters = Array.empty,
        inventoryAgeFilters = Array.empty,
        owningBranchFilters = Array.empty,
                                            -- this is to initialize an Array, repeat creates one item in this case and that lets us push rest of the items
                                            -- this list can be generated off of datasource, when that happens we dont need to hardcode index value, just use indexedMap
                                            -- and set the generated index value to index prop
        expandCollapseSearchFilterStates = Array.repeat 1 {index = 0,searchFilterCustomType = SalesStatus, userAction = False} 
                                                |> Array.push {index = 1,searchFilterCustomType = Year, userAction = False}
                                                |> Array.push {index = 2,searchFilterCustomType = Make, userAction = False}
                                                |> Array.push {index = 3,searchFilterCustomType = MakeModel, userAction = False}
                                                |> Array.push {index = 4,searchFilterCustomType = SleeperRoof, userAction = False}
                                                |> Array.push {index = 5,searchFilterCustomType = SleeperBunk, userAction = False}
                                                |> Array.push {index = 6,searchFilterCustomType = Price, userAction = False}
                                                |> Array.push {index = 7,searchFilterCustomType = EngineHP, userAction = False}
                                                |> Array.push {index = 8,searchFilterCustomType = EngineMake, userAction = False}
                                                |> Array.push {index = 9,searchFilterCustomType = SleeperInches, userAction = False}
                                                |> Array.push {index = 10,searchFilterCustomType = TransType, userAction = False}
                                                |> Array.push {index = 11,searchFilterCustomType = BodyType, userAction = False}
                                                |> Array.push {index = 12,searchFilterCustomType = Suspension, userAction = False}
                                                |> Array.push {index = 13,searchFilterCustomType = RearAxleType, userAction = False}
                                                |> Array.push {index = 14,searchFilterCustomType = WheelBase, userAction = False}
                                                |> Array.push {index = 15,searchFilterCustomType = Mileage, userAction = False}
                                                |> Array.push {index = 16,searchFilterCustomType = FrontAxleWeight, userAction = False}
                                                |> Array.push {index = 17,searchFilterCustomType = RearAxleWeight, userAction = False}
                                                |> Array.push {index = 18,searchFilterCustomType = FleetCode, userAction = False}
                                                |> Array.push {index = 19,searchFilterCustomType = TruckStatus, userAction = False}
                                                |> Array.push {index = 20,searchFilterCustomType = SpecialFinancing, userAction = False}
                                                |> Array.push {index = 21,searchFilterCustomType = InventoryAge, userAction = False}
                                                |> Array.push {index = 22,searchFilterCustomType = OwningBranch, userAction = False},

        --expandCollapseSearchFilterRangeStates = Array.repeat 1 {index = 0,searchFilterRangeUnionType = Price, userAction = True},

        expandCollapseAllChecked = False,
        showDropdown = False,
        currentSortBy = MakeAtoZ
    }