BeginPackage["TraderWorkstation`",{"ExpressionManipulation`", "InternetTools`"}];

(*  User paths and constants  -- you need to uncomment  *)

(* <Begin user settings>

$TWSSettingsFile = ""; (* Path to the tws.xml file in the Jts directory *);

$TWSSecuritiesDirectory = ""; (* Path to where the .cvs files that TWS imports will be stored *)

$TWSAPIJarPath=""; (* Path to the jtsclient.jar file *)

$TWSSecuritiesFile = ""; (* Path to the securities text file *);

$TWSPort = 9999; (* TWS incoming port (Integer, not String) *)

$IBAccountNumber = ""; (* IB account number "UXXXXXX" *)

<End user settings> *)

(* Paths and constants *)

$TWSURL = "http://interactivebrokers.com/en/software/twsDisclaimer.php";

$InteractiveBrokersContractSearchURL =
  "http://www.interactivebrokers.co.uk/contract_info/v3.2/index.php";

(* Initialization *)

startTWS[] :=
    OpenURL[$TWSURL]

initializeTWSJava[] :=
 Catch[If[MatchQ[$TWSJavaInitialized, True], Null,
   Check[AddToClassPath[$TWSAPIJarPath];
    Map[LoadJavaClass[
       StringJoin["com.ib.client.", #1]] &, {"EClientSocket",
      "EWrapper", "ComboLeg", "Contract", "ContractDetails",
      "EClientSocket", "Execution", "ExecutionFilter", "Order",
      "OrderState", "ScannerSubscription", "TickType"}] (*Map*);
	LoadJavaClass["TWSWrapper"];
	$TWSWrapper = JavaNew["TWSWrapper"];
	$TWSSocket = $TWSWrapper[client[]];
	reqId = 1;
    $TWSJavaInitialized = True, Throw@$Failed]
    ]
  ]

twsConnect[pars__] :=
    $TWSWrapper[connect[pars]]

initializeConnection[] :=
    Block[ {},
        twsConnect["", $TWSPort, 1];
        reqID = 1;
    ]

twsDisconnect[] :=
    $TWSWrapper[disconnect[]]

connectedQ[] :=
    $TWSWrapper[isConnected[]]


(* API function structure *)


st = "  /* ***************************************************************\n    * EWrapper\n    *****************************************************************/\n\n   public void tickPrice(int tickerId, int field, double price, int canAutoExecute) {\n      logIn(\"tickPrice\");\n   }\n\n   public void tickSize(int tickerId, int field, int size) {\n      logIn(\"tickSize\");\n   }\n\n   public void tickGeneric(int tickerId, int tickType, double value) {\n      logIn(\"tickGeneric\");\n   }\n\n   public void tickString(int tickerId, int tickType, String value) {\n      logIn(\"tickString\");\n   }\t\n\n   public void tickSnapshotEnd(int tickerId) {\n\t      logIn(\"tickSnapshotEnd\");\n   }\t\n   \n   public void tickOptionComputation(int tickerId, int field, double impliedVol,\n      double delta, double modelPrice, double pvDividend) {\n      logIn(\"tickOptionComputation\");\n   }\t\n\n   public void tickEFP(int tickerId, int tickType, double basisPoints,\n      String formattedBasisPoints, double impliedFuture, int holdDays,\n      String futureExpiry, double dividendImpact, double dividendsToExpiry) {\n      logIn(\"tickEFP\");\n   }\n\n   public void orderStatus(int orderId, String status, int filled, int remaining,\n      double avgFillPrice, int permId, int parentId, double lastFillPrice,\n      int clientId, String whyHeld) {\n      logIn(\"orderStatus\");    \t\n   }\n\n   public void openOrder(int orderId, Contract contract, Order order, OrderState orderState) {\n      logIn(\"openOrder\");\n   }\n\n   public void openOrderEnd() {\n      logIn(\"openOrderEnd\");\n   }\n\n   public void updateAccountValue(String key, String value, String currency, String accountName) {\n      logIn(\"updateAccountValue\");\n   }\n\n   public void updatePortfolio(Contract contract, int position, double marketPrice, double marketValue,\n      double averageCost, double unrealizedPNL, double realizedPNL, String accountName) {\n      logIn(\"updatePortfolio\");\n   }\n\n   public void updateAccountTime(String timeStamp) {\n      logIn(\"updateAccountTime\");\n   }\n\n   public void accountDownloadEnd(String accountName) {\n      logIn(\"accountDownloadEnd\");\n   }\n\n   public void nextValidId(int orderId) {\n      logIn(\"nextValidId\");\n   }\n\n   public void contractDetails(int reqId, ContractDetails contractDetails) {\n      logIn(\"contractDetails\");\n   }\n\n   public void contractDetailsEnd(int reqId) {\n      logIn(\"contractDetailsEnd\");\n   }\n\n   public void bondContractDetails(int reqId, ContractDetails contractDetails) {\n      logIn(\"bondContractDetails\");\n   }\n\n   public void execDetails(int reqId, Contract contract, Execution execution) {\n      logIn(\"execDetails\");\n   }\n\n   public void execDetailsEnd(int reqId) {\n      logIn(\"execDetailsEnd\");\n   }\n\n   public void updateMktDepth(int tickerId, int position, int operation, int side, double price, int size) {\n      logIn(\"updateMktDepth\");\n   }\n\n   public void updateMktDepthL2(int tickerId, int position, String marketMaker, int operation,\n      int side, double price, int size) {\n      logIn(\"updateMktDepthL2\");\n   }\n\n   public void updateNewsBulletin(int msgId, int msgType, String message, String origExchange) {\n      logIn(\"updateNewsBulletin\");\n   }\n\n   public void managedAccounts(String accountsList) {\n      logIn(\"managedAccounts\");\n   }\n\n   public void receiveFA(int faDataType, String xml) {\n      logIn(\"receiveFA\");\n   }\n\n   public void historicalData(int reqId, String date, double open, double high, double low,\n      double close, int volume, int count, double WAP, boolean hasGaps) {\n      logIn(\"historicalData\");\n   }\n\n   public void scannerParameters(String xml) {\n      logIn(\"scannerParameters\");\n   }\n\n   public void scannerData(int reqId, int rank, ContractDetails contractDetails, String distance,\n      String benchmark, String projection, String legsStr) {\n      logIn(\"scannerData\");\n   }\n\n   public void scannerDataEnd(int reqId) {\n      logIn(\"scannerDataEnd\");\n   }\n\n   public void realtimeBar(int reqId, long time, double open, double high, double low, double close, \n      long volume, double wap, int count) {\n      logIn(\"realtimeBar\");\n   }\n\n   public void currentTime(long millis) {\n      logIn(\"currentTime\");\n   }\n\n   public void fundamentalData(int reqId, String data) {\n      logIn(\"fundamentalData\");    \t\n   }\n\n   public void deltaNeutralValidation(int reqId, UnderComp underComp ){\n      logIn(\"deltaNeutralValidation\");    \t\n   }";


Quiet[
    funList = Select[
        ReplaceAll[
            Apply[
                #1 -> Check[StringSplit[StringSplit[#2, ","]], $Failed] & ,
                StringSplit[
                    StringReplace[
                        Part[
                            (StringSplit[#1, " ", 3] & )[
                                StringCases[StringReplace[st, Whitespace -> " "], "public void "~~Shortest[__]~~")"]
                            ],
                            All,
                            3
                        ],
                        ")" -> ""
                    ],
                    "("
                ],
                {1}
            ],
            s_String :> StringTrim[s]
        ],
        FreeQ[#1, $Failed] &
    ];

]

Apply[(argList[#1] = #2) & , funList, {1}];

Apply[(varList[#1] = #2[[All,2]]) & , funList, {1}];

Apply[(typeList[#1] = #2[[All,1]]) & , funList, {1}];

Apply[(argPos[#1] = MapIndexed[#1 -> First[#2] & , #2[[All,2]]]) & , funList, {1}];

(* ================== This is buggy


formatFunction[fun_] :=
    StringJoin[
    Flatten[
    {
    "public void ",
    First[fun],
    " (",
    StringJoin[Riffle[MapThread[StringJoin[#1, " ", #2] & , {typeList[fun], argList[fun]}], ", "]],
    ")",
    "{",
    "\n\tvec = new Vector();",
    "\n\tvec.add(\"",
    head[fun],
    "\"),;",
    "\n",
    ExportString[(StringJoin["\tvec.add(", #1, ");"] & ) /@ argList[fun], "Lines"],
    "\n",
    "\toutputVec.add(vec);",
    "\n}\n"
    }
    ]
    ]

Quiet[StringJoin[Riffle[formatFunction /@ funList, "\n\n"]]; ]

=============== *)

(* Data management *)

vectorContents[v_] :=
    Block[ {l = v[size[]]},
        If[ l == 0,
            Null,
            Table[v[get[i]], {i, 0, l - 1}]
        ]
    ]

getVector[hash_] :=
    Block[ {v = getHash[hash], l},
        If[ MatchQ[v, $Failed],
            $Failed,
            vectorContents[v]
        ]
    ]

logVector :=
    vectorContents /@ vectorContents[$TWSWrapper[logVec]]

outputVector :=
    vectorContents /@ vectorContents[$TWSWrapper[outVec]]

clearOutputVector[] :=
    $TWSWrapper[outVec][clear[]]

clearLogVector[] :=
    $TWSWrapper[logVec][clear[]]

(* Data request *)


twsFunction[method_, args___] :=
    Block[ {arglist = {args}},
        $TWSSocket[method[args]]
    ]

result[id_] :=
    With[ {vec = outputVector},
        Pick[vec, vec[[All,2]], id]
    ]

(* Processing Java output *)


ClearAll[fromJavaExpression, fromJavaObject, javaObjectQ]

javaObjectQ[expr_] :=
    MatchQ[GetClass[expr], _JavaClass]

fromJavaObject[(obj_)?javaObjectQ] :=
    Switch[First[GetClass[obj]],

    "java.util.Vector",
    vectorContents[obj],

    "com.ib.client.Contract",
    contractContents[obj],

    _,
    obj
    ]

fromJavaExpression[expr_] :=
    expr //. s_Symbol /; javaObjectQ[s] :> fromJavaObject[s]

(* Contract manipulation *)

contractVarTypes = Part[
    Map[
        Function[
            Apply[
                Function[
                    StringDrop[#2, 2] -> (
                        ReplaceAll[
                            #1,
                            {
                            "int" -> "Integer",
                            "Vector" -> "Vector",
                            "String" -> "String",
                            "double" -> "Real",
                            "boolean" -> "Boolean"
                            }
                        ]
                    )
                ],
                StringSplit[#1]
            ]
        ],
        ReplaceAll[
            StringSplit[
                "int p_conId, String p_symbol, String p_secType, String p_expiry,\n                double p_strike, String p_right, String p_multiplier,\n                String p_exchange, String p_currency, String p_localSymbol,\n                Vector p_comboLegs, String p_primaryExch, boolean p_includeExpired, \n                String p_secIdType, String p_secId",
                ","
            ],
            s_String :> StringTrim[s]
        ]
    ],
    All,
    2
];


contractVars = {
"ContractID",
"Symbol",
"SecurityType",
"Expiration",
"StrikePrice",
"OptionType",
"Multiplier",
"Exchange",
"Currency",
"LocalSymbol",
"ComboLegs",
"PrimaryExchange",
"IncludeExpired",
"SecurityIDType",
"SecurityID"
};


contractVarTypeRules = Apply[Rule, Transpose[{contractVars, contractVarTypes}], {1}];

contractVarTypeDefaultValues = {"String" -> "", "Integer" -> 0, "Boolean" -> False, "Vector" -> Null, "Real" -> 0.};

contractVarDefaultValues = Apply[Rule, Transpose[{contractVars, contractVarTypes /. contractVarTypeDefaultValues}], {1}];


contractArguments[rules_] :=
    contractVars /. rules /. contractVarDefaultValues

(* MATCHING EXCHANGES AND CURRENCIES IS crucial for data queries -- need to build a complete list *)

exchangeToCurrencyRules = {"FWB" -> "EUR","LSE"->"GBP", "TSEJ"-> "JPY", "SEHK"->"HKD", _String -> "USD"};

ClearAll[contractObject]

contractObject[rules:{__Rule}] :=
    JavaNew["com.ib.client.Contract", Sequence @@ contractArguments[rules]]

toContractRules[
  st_String] := (Which[
     Length[#1] == 1,
     {"Symbol" -> First[#1], "SecurityType" -> "STK",
       "Exchange" -> "ARCA", "Currency" -> "USD"},
     Length[#1] == 2,
     {"Symbol" -> #1[[2]], "SecurityType" -> "STK",
      "Exchange" -> #1[[1]],
      "Currency" -> (#1[[1]] /. exchangeToCurrencyRules)},
     Length[#1] == 3,
     {"Symbol" -> #1[[3]], "SecurityType" -> #1[[1]],
       "Exchange" -> #1[[2]],
      "Currency" -> (#1[[2]] /. exchangeToCurrencyRules)},
     Length[#1] == 4,
     {"Symbol" -> #1[[3]], "SecurityType" -> #1[[1]],
       "Exchange" -> #1[[2]], "Currency" -> #1[[4]]}] &)[
  ToUpperCase /@ StringSplit[st, ":"]]

contractObject[c:Contract[__Rule]] :=
    contractObject[List @@ c]

contractObject[st_String] :=
    contractObject[toContractRules[st]]

st = Identity[
    "java.util.Vector m_comboLegs\nString m_comboLegsDescrip\nint m_conId\nString m_currency\nString m_exchange\nString m_expiry\nboolean m_includeExpired\nString m_localSymbol\nString m_multiplier\nString m_primaryExch\nString m_right\nString m_secId\nString m_secIdType\nString m_secType\ndouble m_strike\nString m_symbol\ncom.ib.client.UnderComp m_underComp"
];


(#1 -> #1 & ) /@ StringReplace[StringSplit[ImportString[st, "Lines"]][[All,2]], "m_" -> ""];

contractFieldVarsRules = {
"ComboLegs" -> "m_comboLegs",
"ComboLegsDescription" -> "m_comboLegsDescrip",
"ContractID" -> "m_conId",
"Currency" -> "m_currency",
"Exchange" -> "m_exchange",
"ExpiryDate" -> "m_expiry",
"IncludeExpired" -> "m_includeExpired",
"LocalSymbol" -> "m_localSymbol",
"Multiplier" -> "m_multiplier",
"PrimaryExchange" -> "m_primaryExch",
"Right" -> "m_right",
"SecurityID" -> "m_secId",
"SecurityIDType" -> "m_secIdType",
"SecurityType" -> "m_secType",
"Strike" -> "m_strike",
"Symbol" -> "m_symbol",
"UnderComp" -> "m_underComp"
};


toJavaSymbol[st_] :=
    Symbol[StringReplace[st, "_" -> "U"]]

contractFieldVars = contractFieldVarsRules[[All,1]];

contractFieldTypes = With[ {typeRules = Apply[Rule, Reverse /@ StringSplit[ImportString[st, "Lines"]], {1}]},
                         (#1 -> (#1 /. contractFieldVarsRules /. typeRules) & ) /@ contractFieldVars
                     ];


contractContents[con_] :=
    Apply[
    Contract,
    DeleteCases[
    Apply[
    Rule,
    Transpose[
    {
    contractFieldVarsRules[[All,1]],
    fromJavaExpression[(con[#1] & ) /@ toJavaSymbol /@ contractFieldVarsRules[[All,2]]]
    }
    ],
    {1}
    ],
    _[_, Null]
    ]
    ]

(* Account report *)


upperFirst[st_String] :=
    StringJoin[ToUpperCase[StringTake[st, 1]], StringTake[st, {2, -1}]]

ClearAll[accountReport]

Options[accountReport] = {"DeleteContractVariables" -> {"ContractID", "IncludeExpired", "Right", "Strike"}};

accountStructureRules = {
{"updateAccountValue", rest__} :>
    "AccountSummary" -> MapAt[If[ StringMatchQ[#1, NumberString],
                                  ToExpression[#1],
                                  #1
                              ] & , Most[{rest}], 2],
{"updatePortfolio", rest__} :>
    "Portfolio" -> Most[Apply[Rule, Transpose[{upperFirst /@ varList["updatePortfolio"], {rest}}], {1}]],
{"updateAccountTime", rest_} :> "AccountTime" -> rest,
{"accountDownloadEnd", __} :> Sequence[]
};


accountReport[OptionsPattern[]] :=
    Block[ {deletePatt = Alternatives @@ OptionValue["DeleteContractVariables"]},
        clearOutputVector[];
        twsFunction[reqAccountUpdates, True, $IBAccountNumber];
        While[Quiet[ !MatchQ[vectorContents[$TWSWrapper[outVec][lastElement[]]], {"accountDownloadEnd", _}]], Null];
        ReplaceAll[
            ReplaceAll[
                (#1[[1,1]] -> #1[[All,2]] & ) /@ GatherBy[fromJavaExpression[outputVector] /. accountStructureRules, First],
                c_Contract :> DeleteCases[c, deletePatt -> _]
            ],
            _["Contract", Contract[rules__]] :> Sequence[rules]
        ]
    ]

accountList[] :=
    "AccountSummary" /. accountReport[]

accountRules[] :=
	First@# -> Rest@# & /@ accountList[]

accountInformation[info_] :=
 With[{rules = #[[1, 1]] -> #[[All, 2]] & /@
     GatherBy[FilterRules[accountRules[], info], First]},
  info /. rules]

accountGrid[] :=
    Text[
    Grid[
    accountList[],
    ItemSize -> Full,
    Background -> {None, {{White, Lighter[Lighter[Gray]]}}},
    Alignment ->
    {
    {Left, ".", Left},
    {Bottom}
    }
    ]
    ]

accountWindow[] :=
    (
    CreateDocument[Panel[accountGrid[]],
    WindowTitle -> "Account Overview", Savable -> False, Magnification -> 1.5, WindowSize -> All];
)

(* Portfolio *)

portfolioList[] :=
    With[ {
    colList = {
    "Symbol",
    "LocalSymbol",
    "SecurityType",
    "PrimaryExchange",
    "Currency",
    "Position",
    "MarketPrice",
    "MarketValue",
    "AverageCost",
    "UnrealizedPNL",
    "RealizedPNL"
    }
    },
        (Prepend[colList /. #1, colList] & )["Portfolio" /. accountReport[]]
    ]

portfolioInformation[info_] :=
	 With[{list = portfolioList[]}, NamedPart[Rest@list, First@list, All, info]]

resolveCurrencies[list_] :=
 Times @@@ list /.
  Reverse /@ Rule @@@ accountInformation["ExchangeRate"] /. Null -> 1

portfolioGrid[] :=
    Text[
    Grid[
    portfolioList[],
    ItemSize -> Full,
    Background -> {None, {{White, Lighter[Lighter[Gray]]}}},
    Alignment ->
    {
    {Left, Left, Left, Left, Left, {"."}},
    {Bottom}
    }
    ]
    ]

portfolioGridDynamic[] :=
DynamicModule[{plist = portfolioList[], list, head, rest, rev, posrules, sortfield, buttons},
head = First@plist;
posrules = PositionRules[head];
  list = Reverse@SortBy[Rest@plist, #[["UnrealizedPNL"/.posrules]] &];
  rev = False;
  buttons =
   Button[Style[#,Small], rev = Not@rev;
      list = With[{sort = # /. posrules},
        If[rev, Identity, Reverse][SortBy[list, #[[sort]] &]]],Appearance -> None] & /@
    head;
  Dynamic@
   Text[Grid[Prepend[list, buttons], Spacings -> {0,Automatic},ItemSize -> Full,
     Background -> {None, {{White, Lighter[Lighter[Gray]]}}},
     Alignment -> {{Left, Left, Left, Left, Left, {"."}}, {Bottom}}]]]


portfolioWindow[] :=
  CreateDocument[
   DynamicModule[{grid = portfolioGridDynamic[]},
    Labeled[Dynamic@Panel[grid],
     Button["Refresh", grid = portfolioGridDynamic[],Appearance->None], {Top, Left}]],
   WindowTitle -> "Portfolio", Magnification -> 1.5, WindowSize -> All, Savable -> False];

(* Account overview *)

accountSummaryList[] :=
 Block[{accountInfo, portfolioInfo,
   accountFields = {"NetLiquidation", "GrossPositionValue",
     "Leverage-S"},
   portfolioFields = {"MarketValue", "UnrealizedPNL", "RealizedPNL"},
   plist = portfolioList[]},
  accountInfo =
   Transpose@{accountFields,
     Total@resolveCurrencies@# & /@
      accountInformation[accountFields]};
  portfolioInfo =
   Transpose@{portfolioFields,
     Total@NamedPart[Rest@plist, First@plist,
       All, {"MarketValue", "UnrealizedPNL", "RealizedPNL"}]};
  Join[accountInfo, portfolioInfo]]


accountSummaryGrid[] :=
    Text[
    Grid[
    accountSummaryList[],
    ItemSize -> Full,
    Background -> {None, {{White, Lighter[Lighter[Gray]]}}},
    Alignment ->
    {
    {Left, "."},
    {Bottom}
    }
    ]
    ]

accountSummaryWindow[] :=
  CreateDocument[
   DynamicModule[{grid = accountSummaryGrid[]},
    Labeled[Dynamic@Panel[grid],
     Button[Style["Refresh",Small], grid = accountSummaryGrid[],Appearance->None], {Top, Left}]],
   WindowTitle -> "Account Summary", Magnification -> 1.5, WindowSize -> All, Savable -> False];

(* Historical data *)

twsDateString[date_, timeZone_:"BRT"] :=
    DateString[
    date,
    {"Year", "Month", "Day", " ", "Hour", "Minute", ":", "Second", " EST"}
    ]

twsDurationString[quant_, unit_] :=
    StringJoin[ToString[quant], " ", StringTake[unit, 1]]

class[object_] :=
    Quiet[Check[First[GetClass[object]], None]]

fromTWSDate[st_] :=
    Switch[Length[StringSplit[st]],

    1,
    ToExpression[(StringTake[st, #1] & ) /@ {{1, 4}, {5, 6}, {7, 8}}],

    2,
    ToExpression[(StringTake[st, #1] & ) /@ {{1, 4}, {5, 6}, {7, 8}, {11, 12}, {14, 15}, {17, 18}}]
    ]

$TWSHistoricalRequestDurationUnits=Characters["SDWMY"];

$TWSHistoricalRequestPeriodUnits=ImportString["1 sec
5 secs
15 secs
30 secs
1 min
2 mins
3 mins
15 mins
30 mins
1 hour
1 day
1 week
1 month
3 months
1 year","Lines"];

ClearAll[reqHist]

reqHist[ticker_, date_, dur_, int_] :=
    reqHist[ticker, date, dur, int, "WAP", "TRADES"]

reqHist[ticker_, date_, dur_, int_, fields_, type_] :=
Catch[Check[
 (* If caching desired *)
 (* reqHist[ticker, date, dur, int, fields, type] = *)
   Block[{expr}, clearOutputVector[];
    twsFunction[reqHistoricalData, ++reqId, contractObject[ticker],
     twsDateString[date], twsDurationString @@ dur, int,
     ToUpperCase[type], 1, 1];
    While[
     And[Or[$TWSWrapper[outVec][size[]] == 0,
       Check[! StringMatchQ[
          vectorContents[$TWSWrapper[outVec][lastElement[]]][[3]],
          "finished*"], True]], !
       MatchQ[vectorContents[$TWSWrapper[logVec][
          lastElement[]]], {"ErrorVector", reqId, __}]],
     Pause[0.01]];
    expr = fromJavaExpression[Most[outputVector]] /. Null -> $Failed;
    Switch[expr, $Failed, Throw[$Failed], _,
     Transpose[{fromTWSDate /@ expr[[All, 3]],
       expr[[All, (fields /. argPos["historicalData"]) +
          1]]}]]], $Failed]]

DataSeriesFunction["TWS"] = Function[
    {secs, date},
    (TimeSeriesTake[reqHist[#1, DateList[][[1 ;; 3]], {1, "Y"}, "1 day"], Most[DateParse[date]]] & ) /@ secs
];

DataSeriesFunction[{"TWS", back_, int_}] :=
 Function[{secs,
   date}, (TimeSeriesTake[
      reqHist[#1, DatePlus[DateList[], {1, "Day"}][[;; 3]], back, int],
      Most[DateParse[date]]] &) /@ secs]

$DataSeriesFunction = "TWS";

(* Market data *)

tickType[i_Integer] :=
    TickType`getField[i]

marketSnapshot[con_] :=
    Catch[
    Block[ {t},
        clearOutputVector[];
        twsFunction[reqMktData, ++reqId, contractObject[con], "", True];
        While[
            And[
                Or[
                    $TWSWrapper[outVec][size[]] == 0,
                     !MatchQ[vectorContents[$TWSWrapper[outVec][lastElement[]]], {"tickSnapshotEnd", reqId}]
                ],
                 !MatchQ[vectorContents[$TWSWrapper[logVec][lastElement[]]], {"ErrorVector", reqId, __}]
            ],
            Pause[0.01]
        ];
        t[1] = Cases[
            Map[
                Function[
                    If[ MatchQ[
                            First[#1],
                            Apply[
                                Alternatives,
                                {
                                "tickPrice",
                                "tickSize",
                                "tickString",
                                "tickGeneric",
                                "tickOptionComputation",
                                "tickEFP"
                                }
                            ]
                        ],
                        Quiet[Check[MapAt[tickType, #1, 3], #1]],
                        #1
                    ]
                ],
                Cases[outputVector, {_, reqId, ___}]
            ],
            {Except["tickSnapshotEnd"], reqId, Except["halted"], ___}
        ];
        Apply[upperFirst[StringTrim[#1, "\""]] -> #2 & , t[1][[All,{3, 4}]], {1}]
    ]
    ]

snapshotGrid[sec_] :=
    With[ {
    cols = {
    "LastPrice",
    "LastSize",
    "BidPrice",
    "BidSize",
    "AskPrice",
    "AskSize",
    "High",
    "Low",
    "Close",
    "Open",
    "Volume",
    "LastTimestamp"
    }
    },
        Text[
            Grid[
                DeleteCases[Transpose[{cols, cols /. marketSnapshot[sec]}], {x_, x_}],
                ItemSize -> Full,
                Background -> {None, {{Lighter[Lighter[Gray]], White}}},
                Alignment ->
                    {
                    {Left, {"."}},
                    {Bottom}
                    }
            ]
        ]
    ]

ClearAll[snapshotWindow]

snapshotWindow[sec_] :=
    Block[ {},
        CreateDocument[
            snapshotGrid[sec],
            WindowFloating -> True,
            WindowTitle -> sec,
            Magnification -> 1.3,
            WindowSize -> {All, All},
            Savable -> False,
            WindowTitle -> sec
        ];
    ]

(* LogVector *)


logVectorWindow[] :=
    (
    CreateDocument[
    Dynamic[
    Refresh[
    Text[
    Grid[
       Reverse[({Column[{Row[Most[#1], " "], Last[#1]}]} & ) /@ logVector],
       ItemSize -> {50, Full},
       Background -> {None, {{Lighter[Lighter[Gray]], White}}},
       Alignment -> Left
    ]
    ],
    UpdateInterval -> 0.5
    ]
    ],
    Magnification -> 1.5,
    WindowSize -> All,
    Savable -> False
    ];

)

(* IB Contract Search *)

ibContractSearch[]:= OpenURL[$InteractiveBrokersContractSearchURL]

niceGrid[expr_, opts___] :=
 Text@Grid[expr, opts, ItemSize -> Full,
   Background -> {None, {{White, Lighter[Lighter[Gray]]}}},
   Alignment -> {Left}]

$InteractiveBrokersURLParameters =
  Fold[StringSplit,
    "wlId=IB&lang=en&action=Advanced+Search&bondIssueDateHigh=&\
bondIssueDateLow=&bondIssuer=&bondMaturityDateHigh=&\
bondMaturityDateLow=&bondType=&collateralType=&conid=0&contractType=&\
couponType=&country=&currency=&description=dow&entityId=&exchange=&\
exchanges=&fundFamily=&futExpDateHigh=&futExpDateLow=&futuresType=&\
hasBond=&hasFut=&hasOpt=&hasWar=&indexType=&initMargin=&investType=&\
maintMargin=&optExerciseStyle=&optExpDateHigh=&optExpDateLow=&\
optStrikeHigh=&optStrikeLow=&secId=&secIdType=&shortMargin=&sortBy=\
description&sortDir=ASC&start=2&stockType=&symbol=&warExerciseStyle=&\
warExpDateHigh=&warExpDateLow=&warIssueDateHigh=&warIssueDateLow=&\
warIssuer=&warRight=&warStrikeHigh=&warStrikeLow=", {"&", "="}][[All,
    1]];

formatInteractiveBrokersURLParameters[pars_] :=
 StringJoin@Riffle[StringJoin[#1, "=", #2] & @@@ pars, "&"]

$IBSecurityTypeOrderingRules =
  PositionRules[{"Forex","Stock"|"Stocks", "Indices/ETFs", "Bonds", "Futures"|"Future","Futures Options", "Options", "IOPTS"|"IOPT", "Warrants"|"Warrant",_}];

requestInteractiveBrokersURL[pars_] :=
 StringJoin[$InteractiveBrokersContractSearchURL, "?",
  formatInteractiveBrokersURLParameters[pars]]

interactiveBrokersSearchList[pars_String]:=
	interactiveBrokersSearchList[interactiveBrokersParse[pars]]

interactiveBrokersParse[s_String]:=
	Prepend[StringSplit[StringSplit[s],":"] /. {{ss_String} :> "description" -> ss}, "action" ->
     "Advanced+Search"]

interactiveBrokersSearchList[pars:{__Rule}] :=
 Catch@Block[{i = 1, res, allres = {}},
   Label["Start"];
   res = Check[
     Part[import[
          requestInteractiveBrokersURL[
           Join[pars, {{"start", ToString[i]}}]],
          "FullData"] //. {} | {("" | Null) ..} :> Sequence[] /.
        s_String :> StringTrim@s, 1, 1, 1,
       3 ;;][[All, {3, 5, 4, 7, 2}]], {}];
   If[res == {},
    Throw[SortBy[
      allres, {(#[[1]] /. $IBSecurityTypeOrderingRules) &, #[[{1, 2,
           3}]] &}]], allres = Join[allres, res]; i = i + 1;
    Goto["Start"]]]

interactiveBrokersSearchGrid[search_] :=
 niceGrid[formatIBLine/@interactiveBrokersSearchList[search]]

formatIBLine[line_] :=
 MapAt[Button[Text@Style[#, Blue],
    Print@StringJoin@
      Flatten@Transpose@{ToString /@ line[[{2, 3, 5}]], {":", " % ",
          ""}}, Appearance -> None] &, line, 3]

interactiveBrokersSearchWindow[c_: "bovespa"] :=
 Block[{},
  CreateDocument[
    Manipulate[
     interactiveBrokersSearchGrid[cont], {{cont, c, "Search"},
      InputField[#1, String, ContinuousAction -> False,
        FieldSize -> {20, 1}] &}, SynchronousUpdating -> False,
     ControlPlacement -> Top, Paneled -> True], Savable -> False,
    WindowTitle -> "Interactive Brokers Contract Search",
    Magnification -> 1.3, WindowSize -> {All, All}];]

(* TWS Settings *)

$TWSSettings = With[ {file = $TWSSettingsFile},
                   Run[StringJoin["sed '1d' ", file, " > ", ToFileName[$TemporaryDirectory,"tws.xml"]]];
                   Import[ToFileName[$TemporaryDirectory,"tws.xml"]]
               ];


$TWSSettingsRules = Block[ {r},
                        $TWSSettings //. XMLElement -> (r[#1, {#2, #3}] & ) /. {} -> Null /. r -> Rule
                    ];

EndPackage[];