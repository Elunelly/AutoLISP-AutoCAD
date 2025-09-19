
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;
; |                                                                                                                                               | ;
; |                                                   HISTORICAL TRACKING FILE OF THE FUNCTION                                                    | ;
; |                                                            --{  Select-Filter  }--                                                            | ;
; |                                                                                                                                               | ;
; +-----------------------------------------------------------------------------------------------------------------------------------------------+ ;



;                                       []-----------------------[] Select-Filter []-----------------------[]                                       ;
;--- Date of creation       > 18/02/2020                                                                                                            ;
;--- Last modification date > 11/07/2022                                                                                                            ;
;--- Author                 > Luna                                                                                                                  ;
;--- Version                > 3.2.1                                                                                                                 ;
;--- Class                  > "DtSel"                                                                                                               ;

;--- Goal and utility of the main function                                                                                                          ;
;   In order to greatly facilitate the selection of dynamic blocks, this function aims to be able to select the blocks (or object) according to     ;
;   different modes and thus return the composition of the selection set according to the specified parameters. It is thus possible to filter the   ;
;   blocks according to their attributes, dynamic properties, EffectiveName, DXF list, etc. It works in a similar way to the (ssget) function, with ;
;   some additions. The return in the command history allows to have a count of the blocks and objects according to their respective properties. Be ;
;   careful though, after several tries, it seems that the function is not able to work correctly when there is a large selection (limit of the     ;
;   number of objects to be defined), generating an "Access violation ..." error.                                                                   ;
;                                                                                                                                                   ;
;--- Declaration of the arguments                                                                                                                   ;
; The function (Select-filter) have 5 argument(s) :                                                                                                 ;
;   --•  mode                   > only 4 modes of selection have been programmed :                                                                  ;
;                     "DXF"   allows you to perform a standard selection (ssget) while adding a count of the objects according to their DXF         ;
;                             properties (specified in argument).                                                                                   ;
;                                                                                                                                                   ;
;                     "BLC"   allows you to select from the EffectiveName of the block reference, thus allowing you to select dynamic and standard  ;
;                             blocks without hindrance. The counting will be done according to the name of each block definition present in the     ;
;                             selection set. If no pre-selection, use the selection mode "_X" of the function (ssget).                              ;
;                                                                                                                                                   ;
;                     "ATT"   allows you to make a selection from the name and/or the value of one or more attributes of block references (standard ;
;                             and/or dynamic). The counting will be done according to the name of the attribute and its value. If no pre-selection, ;
;                             use the selection mode "_X" of the function (ssget).                                                                  ;
;                                                                                                                                                   ;
;                     "DYN"   allows you to make a selection from the name and/or value of one or more dynamic block reference properties (dynamic  ;
;                             only). The count will be done according to the name of the dynamic property and its value. If no pre-selection, use   ;
;                             the selection mode "_X" (ssget).                                                                                      ;
;                                                                                                                                                   ;
;     (type mode) = 'STR                        | Ex. : "DXF", "BLC", "ATT" or "DYN"                                                                ;
;                                                                                                                                                   ;
;   --•  filter                 > depending on the selection 'mode', there's 2 possibilities :                                                      ;
;        | if 'mode = "DXF"   then filter is the list of DXF codes that you want to use for the count. If an object does not have any of the        ;
;                             specified DXF codes, it will not appear in the detailed count, but it will still be present in the selection set and  ;
;                             the object count.                                                                                                     ;
;                                                                                                                                                   ;
;        | if 'mode = "BLC"   then filter is the search key for the EffectiveName of the block. A search key allows the use of Wildcard Characters  ;
;        | or 'mode = "ATT"   to specify multiple block names for example or a relative search, etc. For selection of dynamic blocks, it is         ;
;        | or 'mode = "DYN"   necessary to add the string "`*U*" (except for the "DYN" mode!).                                                      ;
;                                                                                                                                                   ;
;     (type filter) = 'LST                      | Ex. : '(0 8), '("Module*,*PVBlock*"), '("*"), '("Cartouche*" "`*U*"), ...                         ;
;                                                                                                                                                   ;
;   --•  arg-lst                > corresponds to the list of arguments of the function (ssget) for a use of the native function in its full         ;
;                               possibilities. Optional arguments of (ssget) are taken into account in the form of a list ([sel-method] [pt1 [pt2]] ;
;                               [pt-list] [filter-list]). Here is a non-exhaustive list of the selection methods of the (ssget) function, proposed  ;
;                               by LeeMac :                                                                                                         ;
;                                                                                                                                                   ;
;                            •  "_"   -> Non-localised mode string prefix                                                                           ;
;                                   Ensures the English version of the mode string is used in non-English versions of AutoCAD.                      ;
;                                   Not strictly necessary for all mode strings (since some mode strings are the same in all language versions),    ;
;                                   but safer to include than exclude.                                                                              ;
;                                                                                                                                                   ;
;                            •  "+."  -> Point Selection Mode                                                                                       ;
;                                   Forces ssget to remain in 'point' mode, similar to setting PICKAUTO to 0. May be combined with the ":S"         ;
;                                   single-selection mode and ":E" mode to emulate an entsel selection by avoiding implied selection windows.       ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget "_+.:E:S")                                                                              |      ;
;                                        |   Will emulate an (entsel) selection behavior.                                                    |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                   This mode string must be combined with other mode strings and will return a too few arguments error if used on  ;
;                                   its own.                                                                                                        ;
;                                                                                                                                                   ;
;                            •  "A"   -> All                                                                                                        ;
;                                   Similar to the "X" mode string but excludes objects on frozen layers. Selects all objects on thawed layers.     ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |   (ssget "_A" '((0 . "LINE"))), selects all lines residing on thawed layers in the drawing.       |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                   This mode string cannot be combined with graphical selection mode strings.                                      ;
;                                                                                                                                                   ;
;                            •  "B"   -> Box                                                                                                        ;
;                                   Selects all objects inside or crossing a rectangle specified by two points. If the points are specified from    ;
;                                   right to left, Box is equivalent to Crossing. Otherwise, Box is equivalent to Window.                           ;
;                                                                                                                                                   ;
;                            •  "C"   -> Crossing                                                                                                   ;
;                                   Selects objects residing within and/or crossing a rectangular area defined by two points. The supplied points   ;
;                                   should be expressed relative to the UCS, however, the crossing window will always be orthogonal to the WCS axes.;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget "_C" '(0 0) '(2 1) '((0 . "CIRCLE")))                                                   |      ;
;                                        |   Selects all circles residing inside or crossing the rectangle with vertices at (0,0), (2,0),    |      ;
;                                        |   (2,1) and (0,1).                                                                                |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                   Caution: Only objects visible in the drawing area at the time of selection will be selected by this method.     ;
;                                   Consider temporarily zooming to the selection window before performing the selection, and then zoom previous to ;
;                                   restore the original screen position.                                                                           ;
;                                                                                                                                                   ;
;                            •  "CP"  -> Crossing Polygon                                                                                           ;
;                                   Selects objects residing within and/or crossing a polygon defined by a list of UCS points. The polygon can be   ;
;                                   any shape but cannot cross or touch itself. AutoCAD will construct the last segment of the polygon to ensure it ;
;                                   is closed at all times.                                                                                         ;
;                                   The CP mode string is not affected by the PICKADD System Variable.                                              ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget "_CP" '((1 1) (3 1) (5 2) (2 4)))                                                       |      ;
;                                        |   Selects all objects residing inside or crossing a polygon with vertices at (1,1), (3,1), (5,2)  |      ;
;                                        |   and (2,4).                                                                                      |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                   Caution: only objects visible in the drawing area at the time of selection will be selected by this method.     ;
;                                   Consider temporarily zooming to the selection area before performing the selection, and then zoom previous to   ;
;                                   restore the original screen position.                                                                           ;
;                                                                                                                                                   ;
;                            •  ":D"  -> Allow Duplicates                                                                                           ;
;                                   Includes duplicate selected entities in the selection, else duplicates are ignored.                             ;
;                                                                                                                                                   ;
;                            •  ":E"  -> Everything within Aperture                                                                                 ;
;                                   Allows selection of everything within the cursor's object selection pickbox. This mode may be used in           ;
;                                   conjunction with the "+." and ":S" mode strings to emulate entsel selection behaviour.                          ;
;                                                                                                                                                   ;
;                            •  "F"   -> Fence                                                                                                      ;
;                                   Selects all objects crossing a selection fence. The Fence method is similar to CP (Crossing Polygon) except     ;
;                                   that AutoCAD does not close the fence, and a fence can cross itself.                                            ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget "_F" '((0 0) (1 1)) '((0 . "LINE")))                                                    |      ;
;                                        |   Selects all lines crossing the fence line running from (0,0) to (1,1).                          |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                   Fence is not affected by the PICKADD System Variable.                                                           ;
;                                   Caution: Only objects visible in the drawing area at the time of selection will be selected by this method.     ;
;                                   Consider temporarily zooming to the selection window before performing the selection, and then zoom previous to ;
;                                   restore the original screen position.                                                                           ;
;                                                                                                                                                   ;
;                            •  "G"   -> Groups                                                                                                     ;
;                                   Selects all objects within a specified group. Although a valid ssget mode string, there is no provision for     ;
;                                   passing group names as arguments to the ssget function and hence this option is only of use at the command-line ;
;                                   during the Select objects prompt.                                                                               ;
;                                                                                                                                                   ;
;                            •  "I"   -> Implied                                                                                                    ;
;                                   Implied selection (objects selected while PICKFIRST is in effect).                                              ;
;                                                                                                                                                   ;
;                            •  "L"   -> Last                                                                                                       ;
;                                   Selects the last visible object added to the drawing database.                                                  ;
;                                   Caution: when using the "L" selection method in an MDI environment, you cannot always count on the last object  ;
;                                   drawn to remain visible. For example, if your application draws a line, and the user subsequently minimizes or  ;
;                                   cascades the AutoCAD drawing window, the line may no longer be visible. If this occurs, ssget with the "L"      ;
;                                   option will return nil.                                                                                         ;
;                                                                                                                                                   ;
;                            •  ":L"  -> Exclude Locked Layer                                                                                       ;
;                                   Rejects selection of objects residing on locked layers.                                                         ;
;                                                                                                                                                   ;
;                            •  "M"   -> Multiple                                                                                                   ;
;                                   Specifies multiple points without highlighting the objects, thus speeding up the selection process for complex  ;
;                                   objects. The Multiple method also selects two intersecting objects if the intersection point is specified twice.;
;                                                                                                                                                   ;
;                            •  ":N"  -> Nested                                                                                                     ;
;                                   Call ssnamex for additional information on container blocks and transformation matrices for any entities        ;
;                                   selected during the ssget operation. This additional information is available only for entities selected via    ;
;                                   graphical selection methods such as Window, Crossing, and point picks.                                          ;
;                                   Unlike the other object selection methods, :N may return multiple entities with the same entity name in the     ;
;                                   selection set. For example, if the user selects a subentity of a complex entity such as a BlockReference,       ;
;                                   PolygonMesh, or old style polyline, ssget looks at the subentity that is selected when determining if it has    ;
;                                   already been selected. However, ssget actually adds the main entity (BlockReference, PolygonMesh, etc.) to the  ;
;                                   selection set. The result could be multiple entries with the same entity name in the selection set (each will   ;
;                                   have different subentity information for ssnamex to report).                                                    ;
;                                   This ssget mode string is known to perform erratically.                                                         ;
;                                                                                                                                                   ;
;                            •  "P"   -> Previous                                                                                                   ;
;                                   Selects the most recent selection set.                                                                          ;
;                                   The Previous selection set is cleared by operations that delete objects from the drawing. AutoCAD keeps track   ;
;                                   of whether each selection set was specified in model space or paper space. The Previous selection set is        ;
;                                   ignored if you switch spaces.                                                                                   ;
;                                                                                                                                                   ;
;                            •  ":P"  -> Reject Viewports                                                                                           ;
;                                   Rejects selection of Viewport objects.                                                                          ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Equivalent to:                                                                                  |      ;
;                                        |    (ssget '((0 . "~VIEWPORT")))                                                                   |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  ":R"  -> Permit Long Transaction                                                                                    ;
;                                   Allows entities in a long transaction to be selected.                                                           ;
;                                                                                                                                                   ;
;                            •  ":S"  -> Single Selection                                                                                           ;
;                                   The user is permitted a single attempt to make a selection of objects using any available selection method.     ;
;                                   When combined with either "+." or ":E", only a single object may be selected, emulating an entsel selection.    ;
;                                                                                                                                                   ;
;                            •  ":U"  -> Enable Subentity Selection -2006+                                                                          ;
;                                   Cannot be combined with the duplicate (":D") or nested (":N") selection modes.                                  ;
;                                   In this mode, top-level entities are selected by default, but the user can attempt to select subentities by     ;
;                                   pressing the CTRL key while making the selection. This option is supported only with interactive selections,    ;
;                                   such as window, crossing, and polygon. It is not supported for all, filtered, or group selections.              ;
;                                                                                                                                                   ;
;                            •  ":V"  -> Force Subentity Selection -2006+                                                                           ;
;                                   Treats all interactive, graphic selections performed by the user as subentity selections.                       ;
;                                   The returned selection set contains subentities only. This option cannot be combined with the duplicate (":D")  ;
;                                   or nested (":N") selection modes. This option is supported only with interactive selections, such as window and ;
;                                   crossing. It is not supported for all, filtered, or group selections.                                           ;
;                                                                                                                                                   ;
;                            •  "W"   -> Window                                                                                                     ;
;                                   Selects all objects residing completely inside a rectangle defined by two UCS points.                           ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget "_W" '(3 2) '(5 4) '((0 . "TEXT")))                                                     |      ;
;                                        |   Selects all text objects residing entirely inside the rectangle with vertices at (3,2), (5,2),  |      ;
;                                        |   (5,4) and (3,4).                                                                                |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                   Caution: Only objects visible in the drawing area at the time of selection will be selected by this method.     ;
;                                   Consider temporarily zooming to the selection window before performing the selection, and then zoom previous to ;
;                                   restore the original screen position.                                                                           ;
;                                                                                                                                                   ;
;                            •  "WP"  -> Window Polygon                                                                                             ;
;                                   Selects objects completely inside a polygon defined by a list of points. The polygon can be any shape but can't ;
;                                   cross or touch itself. AutoCAD will construct the last segment of the polygon to ensure it is closed at all     ;
;                                   times. WPolygon is not affected by the PICKADD System Variable.                                                 ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget "_WP" '((0 1) (4 2) (6 4)) '((0 . "ARC") (40 . 1.0)))                                   |      ;
;                                        |   Selects all arcs with radius 1.0 residing entirely within a polygon with vertices at (0,1),     |      ;
;                                        |   (4,2) and (6,4).                                                                                |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                   Caution: Only objects visible in the drawing area at the time of selection will be selected by this method.     ;
;                                   Consider temporarily zooming to the selection window before performing the selection, and then zoom previous to ;
;                                   restore the original screen position.                                                                           ;
;                                                                                                                                                   ;
;                            •  "X"   -> Extended search (Entire Drawing Database)                                                                  ;
;                                   Iterates over the entire drawing database selection all entities matching the criteria given by the supplied    ;
;                                   filter list (if present); includes entities on layers that are off, frozen & locked, and entities outside of    ;
;                                   the visible drawing area.                                                                                       ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget "_X" '((0 . "CIRCLE")))                                                                 |      ;
;                                        |   Selects all circles in every drawing layout.                                                    |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                     FILTER LIST OPERATORS                                                                                                         ;
;                     Relational Operators                                                                                                          ;
;                                                                                                                                                   ;
;                         • The following relational operators are only valid for use with groups containing integer or real values; the bitwise    ;
;                           operators are limited to integer values only. Use wildcard patterns for testing strings.                                ;
;                         • For point groups, the X, Y, and Z tests can be combined into a single string, with each operator separated by commas    ;
;                           (for example, ">,>,*"). If an operator is omitted from the string (for example, "=,<>" leaves out the Z test), then the ;
;                           'anything goes' operator, "*", is assumed.                                                                              ;
;                         • Direction vectors (DXF Group code 210) can be compared only with the operators "*", "=", and "!=" (or one of the        ;
;                           equivalent 'not equal' strings).                                                                                        ;
;                                                                                                                                                   ;
;                            •  "*"   -> Anything goes (always true)                                                                                ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "LINE") (-4 . "=,*,=") (10 1.0 0.0 1.0)))                                        |      ;
;                                        |   Select lines with start point passing through (1.0,*,1.0) i.e. with any Y-coordinate.           |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "="   -> Equal                                                                                                      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "CIRCLE") (-4 . "*,*,=") (10 0.0 0.0 4.0)))                                      |      ;
;                                        |   Select circles with a center at an elevation of 4.0 units.                                      |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "!="  -> Not Equal                                                                                                  ;
;                                   All three listed operators are equivalent ("!=", "/=", "<>").                                                   ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "CIRCLE") (-4 . "*,*,=") (10 0.0 0.0 4.0)))                                      |      ;
;                                        |   Select circles with a center at an elevation of 4.0 units.                                      |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "<"   -> Less Than                                                                                                  ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "ARC") (-4 . "<") (40 . 2.0)))                                                   |      ;
;                                        |   Select arcs with a radius less than 2.0.                                                        |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "<="  -> Less Than or Equal to                                                                                      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "TEXT") (-4 . "*,<=") (11 0.0 10.0 0.0)))                                        |      ;
;                                        |   Select text with alignment point with Y-coordinate less than or equal to 10.0 units, with any   |      ;
;                                        |   X & Z coordinate value.                                                                         |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  ">"   -> Greater Than                                                                                               ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "LINE") (-4 . ">,>") (11 3.0 3.0 0.0)))                                          |      ;
;                                        |   Select lines with end point passing through a point with X & Y coordinate greater than          |      ;
;                                        |   (3.0,3.0), with any Z-coordinate value.                                                         |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  ">="  -> Greater Than or Equal to                                                                                   ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "POINT") (-4 . "<,>=,=") (10 5.0 7.0 0.0)))                                      |      ;
;                                        |   Select points with X-coordinate less than 5.0, Y-coordinate greater than or equal to 7.0, and   |      ;
;                                        |   with Z-coordinate equal to 0.0.                                                                 |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "&"   -> Bitwise AND (integer groups only)                                                                          ;
;                                   Equivalent to: (/= 0 (logand bit filter)).                                                                      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "POLYLINE") (-4 . "&") (70 . 6)))                                                |      ;
;                                        |   Select Polylines with either curve-fit (2) or spline-fit (4) vertices added.                    |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "&="  -> Bitwise Masked Equals (integer groups only)                                                                ;
;                                   Equivalent to: (= filter (logand bit filter)).                                                                  ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "LWPOLYLINE") (-4 . "&=") (70 . 1)))                                             |      ;
;                                        |   Select closed LWPolylines.                                                                      |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                     Relational Operators                                                                                                          ;
;                                                                                                                                                   ;
;                            •  "<AND...AND>"   -> Logical AND                                                                                      ;
;                                   Matches all enclosed expressions.                                                                               ;
;                                   Since the ssget filter list has an implied AND operator (matching all supplied items), this operator is mostly  ;
;                                   used in conjunction with other logical operators to form compound filters.                                      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget                                                                                         |      ;
;                                        |      '(                                                                                           |      ;
;                                        |        (-4 . "<OR")                                                                               |      ;
;                                        |          (-4 . "<AND") (0 . "ARC")    (40 . 1.5) (-4 . "AND>")                                    |      ;
;                                        |          (-4 . "<AND") (0 . "CIRCLE") (40 . 2.0) (-4 . "AND>")                                    |      ;
;                                        |        (-4 . "OR>")                                                                               |      ;
;                                        |       )                                                                                           |      ;
;                                        |    )                                                                                              |      ;
;                                        |   Selects arcs with radius 1.5 or circles with radius 2.0.                                        |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "<OR...OR>"     -> Logical Inclusive OR                                                                             ;
;                                   Matches one or more enclosed expressions.                                                                       ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "TEXT") (-4 . "<OR") (40 . 1.0) (8 . "0") (62 . 3) (-4 . "OR>")))                |      ;
;                                        |   Selects text objects with text height of 1.0 or layer "0", or with colour set to 3 (green), or  |      ;
;                                        |   a combination of all of these properties.                                                       |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "<XOR...XOR>"   -> Logical Exclusive OR                                                                             ;
;                                   Matches one of TWO enclosed expressions.                                                                        ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "TEXT") (-4 . "<XOR") (40 . 1.0) (62 . 3) (-4 . "XOR>")))                        |      ;
;                                        |   Selects text with text height of 1.0 or with colour set to 3, but not both.                     |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;                            •  "<NOT...NOT>"   -> Logical Not                                                                                      ;
;                                   Rejects objects matching the single enclosed expression.                                                        ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                        |   Example:                                                                                        |      ;
;                                        |    (ssget '((0 . "LINE") (-4 . "<NOT") (62 . 256) (-4 . "NOT>")))                                 |      ;
;                                        |   Selects lines with colour not set to 256 (ByLayer).                                             |      ;
;                                        +---------------------------------------------------------------------------------------------------+      ;
;                                                                                                                                                   ;
;     (type arg-lst) = 'LST                     | Ex. : '("_X" ((0 . "*LINE") (62 . 1) (70 . 0))), '(()), ...                                       ;
;                                                                                                                                                   ;
;   --•  flg-lst                > corresponds to the list of attributes/properties as a list of pointed pairs (tag . value) to filter the selection ;
;                               according to the specified mode. Depending on the selection mode chosen, the pointed pairs will be compared to      ;
;                               different possible lists:                                                                                           ;
;                     "DXF"   flg-lst is compared to the set of properties of the object corresponding to (append (get-att-list) (get-dyn-list))    ;
;                             in order to allow a filter on all properties other than the DXF list.                                                 ;
;                                                                                                                                                   ;
;                     "BLC"   flg-lst is compared to the list of object properties available via the function (vlax-dump-object) to allow filtering ;
;                             on properties not accessible from the DXF list of the object (cf. filter-list (ssget)).                               ;
;                                                                                                                                                   ;
;                     "ATT"   flg-lst is compared to the list of attributes of the object (get-att-list) to allow a filter on the name and/or value ;
;                             of one or more attributes.                                                                                            ;
;                                                                                                                                                   ;
;                     "DYN"   flg-lst is compared to the list of attributes of the object (get-dyn-list) to allow a filter on the name and/or       ;
;                             value of one or more dynamic properties.                                                                              ;
;                                                                                                                                                   ;
;     (type flg-lst) = 'LST                     | Ex. : nil, '(("Num*" . "##") ("A" . "[0-9]")), ...                                                ;
;                                                                                                                                                   ;
;   --•  flag                   > allows to display or not the count of the objects                                                                 ;
;     (type flag) = 'SYM                        | Ex. : T if we want to display the counting of objects, nil otherwise                              ;
;                                                                                                                                                   ;
;--- List of dependent's functions                                                                                                                  ;
;   --•  "UtDac" ---> str2lst                                       | v1.0.0 - 15/04/2017 ((gile))                                                  ;
;   --•  "UtDac" ---> lst2str                                       | v1.1.0 - 01/02/2022 (Luna)                                                    ;
;   --•  "VlPrp" ---> LM:vlax-dump-object->list                     | v2.0.0 - 11/07/2022 (LeeMac)                                                  ;
;   --•  "VlPrp" ---> get-att-list                                  | v2.0.0 - 04/01/2022 (LeeMac)                                                  ;
;   --•  "VlPrp" ---> get-dyn-list                                  | v3.0.1 - 30/01/2022 (LeeMac)                                                  ;
;                                                                                                                                                   ;
;--- List of local functions                                                                                                                        ;
;   --•  "BaLst" ---> check-list                                    | v1.2.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> KeyList-Gen                                   | v1.2.0 - 27/04/2022 (Luna)                                                    ;
;   --•  "BaLst" ---> modf-ss                                       | v1.2.0 - 27/04/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- List of programs using this function                                                                                                           ;
;   --•  "PsUcart" ---> c:DATECART                                  | v2.0.4 - 04/07/2022 (Luna)                                                    ;
;   --•  "PsUcart" ---> c:NAMECART                                  | v6.1.4 - 08/08/2022 (Luna)                                                    ;
;                                                                                                                                                   ;
;--- Return                                                                                                                                         ;
;   The function (Select-Filter) returns the selection set filtered according to the criteria, as well as the counting of the objects in the        ;
;   command history according to the mode defined in argument and 'flag' value.                                                                     ;
;     Ex. : (select-filter "BLC" '("*") nil nil T) returns <Selection set: 217d> as well as the following count in the command history (case of a   ;
;           pre-selection of objects before launching the function to break down the selection set according to the name of the blocks) :           ;
;           Number of selected object(s) = 1116u                                                                                                    ;
;           List of found block(s) :                                                                                                                ;
;             - "Module FS 2000x992x40" (448u - 40.14%)                                                                                             ;
;             - "SUNGROW - Eclisse" (162u - 14.52%)                                                                                                 ;
;             - "SUNGROW - Fixation structure primaire" (16u - 1.43%)                                                                               ;
;             - "SUNGROW - Flotteur (Ext.)" (40u - 3.58%)                                                                                           ;
;             - "SUNGROW - Flotteur (Int.)" (224u - 20.07%)                                                                                         ;
;             - "SUNGROW - Structure d'arrimage (E-W)" (8u - 0.72%)                                                                                 ;
;             - "SUNGROW - Structure d'arrimage (N-S)" (18u - 1.61%)                                                                                ;
;             - "SUNGROW - Structure primaire (6.37m)" (120u - 10.75%)                                                                              ;
;             - "SUNGROW - Structure secondaire (4.25m)" (16u - 1.43%)                                                                              ;
;             - "SUNGROW - Structure secondaire (6.38m)" (64u - 5.73%)                                                                              ;
;                                                                                                                                                   ;
;           (select-filter "ATT" '("*") '("_X") '(("A" . "*")) T) returns <Selection set: 219d> as well as (selection of all blocks with an         ;
;           attribute named "A", regardless of its value) :                                                                                         ;
;           Number of selected object(s) = 97u                                                                                                      ;
;           List of found attribute(s) :                                                                                                            ;
;             - "A" = 01 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 02 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 03 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 04 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 05 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 06 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 07 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 08 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 09 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 1 (8u - 8.25%)                                                                                                                ;
;                -> "Marker" (8u - 100%)                                                                                                            ;
;             - "A" = 10 (7u - 7.22%)                                                                                                               ;
;                -> "Marker" (7u - 100%)                                                                                                            ;
;             - "A" = 11 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 12 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 13 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 14 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 15 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 16 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 17 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 18 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 19 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 2 (6u - 6.19%)                                                                                                                ;
;                -> "Marker" (6u - 100%)                                                                                                            ;
;             - "A" = 20 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 21 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 22 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 23 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 24 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 25 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 26 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 27 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 28 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 29 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 3 (6u - 6.19%)                                                                                                                ;
;                -> "Marker" (6u - 100%)                                                                                                            ;
;             - "A" = 30 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 31 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 32 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 33 (1u - 1.03%)                                                                                                               ;
;                -> "Marker" (1u - 100%)                                                                                                            ;
;             - "A" = 4 (6u - 6.19%)                                                                                                                ;
;                -> "Marker" (6u - 100%)                                                                                                            ;
;             - "A" = 5 (6u - 6.19%)                                                                                                                ;
;                -> "Marker" (6u - 100%)                                                                                                            ;
;             - "A" = 6 (6u - 6.19%)                                                                                                                ;
;                -> "Marker" (6u - 100%)                                                                                                            ;
;             - "A" = 7 (6u - 6.19%)                                                                                                                ;
;                -> "Marker" (6u - 100%)                                                                                                            ;
;             - "A" = 8 (6u - 6.19%)                                                                                                                ;
;                -> "Marker" (6u - 100%)                                                                                                            ;
;             - "A" = 9 (6u - 6.19%)                                                                                                                ;
;                -> "Marker" (6u - 100%)                                                                                                            ;
;             - "A" = C (2u - 2.06%)                                                                                                                ;
;                -> "Marker" (2u - 100%)                                                                                                            ;
;                                                                                                                                                   ;
;           (select-filter "DXF" nil nil '((8 . "UBS-*")) T) returns <Selection set: 21a8> as well as (object pre-selection to keep only objects    ;
;           present on layers whose name starts with "UBS-") and strictly equivalent to (select-filter "DXF" '(8) '(((8 . "UBS-*"))) nil T) :       ;
;           Number of selected object(s) = 2207u                                                                                                    ;
;           List of found object(s) properties :                                                                                                    ;
;             - 8 = UBS-100-Cotation (20u - 0.91%)                                                                                                  ;
;                -> "DIMENSION" (20u - 100%)                                                                                                        ;
;             - 8 = UBS-100-Ensemble structurel (Ponton) (113u - 5.12%)                                                                             ;
;                -> "INSERT" (113u - 100%)                                                                                                          ;
;             - 8 = UBS-100-Modules (48u - 2.17%)                                                                                                   ;
;                -> "LWPOLYLINE" (48u - 100%)                                                                                                       ;
;             - 8 = UBS-100-Structure eclisse (32u - 1.45%)                                                                                         ;
;                -> "INSERT" (32u - 100%)                                                                                                           ;
;             - 8 = UBS-100-Structure flottante (Ext.) (31u - 1.4%)                                                                                 ;
;                -> "INSERT" (31u - 100%)                                                                                                           ;
;             - 8 = UBS-100-Structure flottante (Int.) (28u - 1.27%)                                                                                ;
;                -> "INSERT" (28u - 100%)                                                                                                           ;
;             - 8 = UBS-100-Structure primaire (6.37m) (20u - 0.91%)                                                                                ;
;                -> "INSERT" (20u - 100%)                                                                                                           ;
;             - 8 = UBS-100-Structure primaire (7.13m) (1u - 0.05%)                                                                                 ;
;                -> "INSERT" (1u - 100%)                                                                                                            ;
;             - 8 = UBS-100-Structure secondaire (4.25m) (12u - 0.54%)                                                                              ;
;                -> "INSERT" (12u - 100%)                                                                                                           ;
;             - 8 = UBS-300-Trench (Coupes) (1872u - 84.82%)                                                                                        ;
;                -> "ARC" (534u - 28.53%)                                                                                                           ;
;                -> "DIMENSION" (45u - 2.4%)                                                                                                        ;
;                -> "HATCH" (15u - 0.8%)                                                                                                            ;
;                -> "INSERT" (109u - 5.82%)                                                                                                         ;
;                -> "LWPOLYLINE" (1124u - 60.04%)                                                                                                   ;
;                -> "MTEXT" (7u - 0.37%)                                                                                                            ;
;                -> "MULTILEADER" (32u - 1.71%)                                                                                                     ;
;                -> "SOLID" (6u - 0.32%)                                                                                                            ;
;             - 8 = UBS-900-Viewport (30u - 1.36%)                                                                                                  ;
;                -> "CIRCLE" (16u - 53.33%)                                                                                                         ;
;                -> "LWPOLYLINE" (13u - 43.33%)                                                                                                     ;
;                -> "MTEXT" (1u - 3.33%)                                                                                                            ;
;                                                                                                                                                   ;
;           (select-filter "BLC" '("Module*") '(((8 . "0,UBS-100-*"))) nil T) returns <Selection set: 21bf> as well as (no pre-selection so only    ;
;           blocks whose name starts with "Module" and which are located on layers "0" or starting with "UBS-100-") :                               ;
;           Select objects: Specify the opposite corner: 4233 found, 9 groups                                                                       ;
;           Select objects:                                                                                                                         ;
;           Number of selected object(s) = 4032u                                                                                                    ;
;           List of found block(s) :                                                                                                                ;
;             - "Module FS 2000x992x40" (4032u - 100%)                                                                                              ;
;                                                                                                                                                   ;
;           (select-filter "BLC" '("Module*") '(((8 . "0,UBS-100-*"))) nil nil) returns <Selection set: 21bf> and nothing else because 'flag' is    ;
;           set on nil, wich means that we don't want to display the count of selected objects.                                                     ;
;                                                                                                                                                   ;
;--- Historic list of the version with their modification status                                                                                    ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.2.1   |   Changing the function name (vlax-dump-object->list) from LeeMac as (LM:vlax-dump-object->list)                                 | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.2.0   |   Adding (strcase) function to check the name of the block and avoid problems                                                    | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.1.1   |   Translation of the prompted text in english to avoid any issue for the future                                                  | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.1.0   |   Removal of the mandatory selection of dynamic blocks for the "BLC" and "ATT" methods, now requiring the addition of the        | ;
; |            |   string "`*U*" to recover the previous operation                                                                                | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v3.0.0   |   Total redesign of the function to allow a more complete and richer use. Added the 'flag' argument                              | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v2.0.0   |   ...                                                                                                                            | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
; |   v1.0.0   |   Creation of the function                                                                                                       | ;
; +------------+----------------------------------------------------------------------------------------------------------------------------------+ ;
;                                                                                                                                                   ;

(defun Select-Filter  (mode filter arg-lst flg-lst flag / jsel name vl-name i n e key-lst tmp-lst lst modf-ss KeyList-Gen check-list)
  (defun modf-ss (arg-lst dxf add / tmp-lst)
    (cond
      ( (and
          (listp (last arg-lst))
          (setq tmp-lst (assoc dxf (last arg-lst)))
        )
        (subst
          (subst (cons dxf add) tmp-lst (last arg-lst))
          (last arg-lst)
          arg-lst
        )
      )
      ( (and
          (listp (last arg-lst))
          (not tmp-lst)
        )
        (subst
          (append (last arg-lst) (list (cons dxf add)))
          (last arg-lst)
          arg-lst
        )
      )
      ( T (append arg-lst (list (list (cons dxf add)))))
    )
  )
  
  (defun check-list (ppt-lst lst / tag value)
    (vl-remove-if-not
      '(lambda (ppt)
        (member T
          (mapcar
            '(lambda (x)
              (and
                (wcmatch
                  (strcase (vl-princ-to-string (car ppt)))
                  (strcase (vl-princ-to-string (car x)))
                )
                (wcmatch
                  (strcase (vl-princ-to-string (cdr ppt)))
                  (strcase (vl-princ-to-string (cdr x)))
                )
              )
             )
            lst
          )
        )
       )
      ppt-lst
    )
  )
  
  (defun KeyList-Gen (key lst / sub)
    (if (cdr key)
      (if (setq sub (assoc (car key) lst))
        (subst
          (cons (car key) (KeyList-Gen (cdr key) (cdr sub)))
          sub
          lst
        )
        (cons
          (cons (car key) (KeyList-Gen (cdr key) (cdr sub)))
          lst
        )
      )
      (if (setq sub (assoc (car key) lst))
        (subst (cons (car key) (1+ (cdr sub))) sub lst)
        (cons (cons (car key) 1) lst)
      )
    )
  )

  (vl-load-com)
  (cond
    ( (= mode "BLC")
      (setq
        arg-lst (modf-ss arg-lst 2 (lst2str filter ","))
        arg-lst (modf-ss arg-lst 0 "INSERT")
      )
    )
    ( (= mode "ATT")
      (setq
        arg-lst (modf-ss arg-lst 2 (lst2str filter ","))
        arg-lst (modf-ss arg-lst 0 "INSERT")
        arg-lst (modf-ss arg-lst 66 1)
      )
    )
    ( (= mode "DYN")
      (setq
        arg-lst (modf-ss arg-lst 2 "`*U*")
        arg-lst (modf-ss arg-lst 0 "INSERT")
      )
    )
    ( (= mode "DXF")
      (foreach dxf filter
        (setq flg-lst (car (modf-ss (list flg-lst) dxf "*")))
      )
    )
  )
  (if
    (and
      (setq jsel (vl-catch-all-apply 'ssget arg-lst))
      (not (vl-catch-all-error-p jsel))
    )
    (progn
      (repeat (setq i (sslength jsel))
        (setq
          name (ssname jsel (setq i (1- i)))
          vl-name (vlax-ename->vla-object name)
        )
        (cond
          ( (and
              (= mode "BLC")
              (vlax-property-available-p vl-name 'EffectiveName)
              (wcmatch (strcase (vla-get-EffectiveName vl-name)) (strcase (lst2str filter ",")))
              (if flg-lst (setq lst (check-list (LM:vlax-dump-object->list vl-name) flg-lst)) T)
            )
            (setq key-lst (KeyList-Gen (list (vla-get-EffectiveName vl-name)) key-lst))
          )
          ( (and
              (= mode "ATT")
              (vlax-property-available-p vl-name 'EffectiveName)
              (wcmatch (strcase (vla-get-EffectiveName vl-name)) (strcase (lst2str filter ",")))
              (setq lst
                (if flg-lst
                  (check-list (get-Att-list vl-name) flg-lst)
                  (get-Att-list vl-name)
                )
              )
            )
            (foreach att lst
              (setq key-lst (KeyList-Gen (list att (vla-get-EffectiveName vl-name)) key-lst))
            )
          )
          ( (and
              (= mode "DYN")
              (vlax-property-available-p vl-name 'EffectiveName)
              (wcmatch (strcase (vla-get-EffectiveName vl-name)) (strcase (lst2str filter ",")))
              (= 1 (getpropertyvalue name "IsDynamicBlock"))
              (setq lst
                (if flg-lst
                  (check-list (get-Dyn-list vl-name) flg-lst)
                  (get-Dyn-list vl-name)
                )
              )
            )
            (foreach dyn lst
              (setq key-lst (KeyList-Gen (list dyn (vla-get-EffectiveName vl-name)) key-lst))
            )
          )
          ( (and
              (= mode "DXF")
              (if flg-lst
                (setq lst
                  (check-list
                    (append
                      (entget name)
                      (if
                        (and
                          (= "INSERT" (cdr (assoc 0 (entget name))))
                          (assoc 66 (entget name))
                        )
                        (get-att-list vl-name)
                      )
                      (if
                        (and
                          (= "INSERT" (cdr (assoc 0 (entget name))))
                          (= 1 (getpropertyvalue name "IsDynamicBlock"))
                        )
                        (get-Dyn-list vl-name)
                      )
                    )
                    flg-lst
                  )
                )
                (assoc 0 (entget name))
              )
            )
            (foreach dxf lst
              (setq key-lst
                (KeyList-Gen
                  (if (= (car dxf) 0)
                    (list dxf)
                    (list dxf (cdr (assoc 0 (entget name))))
                  )
                  key-lst
                )
              )
            )
          )
          ( T (ssdel name jsel))
        )
      )
      (if (and flag (> (sslength jsel) 0))
        (progn
          (prompt
            (strcat 
              "\nNumber of selected object(s) = "
              (itoa (sslength jsel))
              "u"
              (cond
                ( (= mode "BLC") "\nList of found block(s) :")
                ( (= mode "ATT") "\nList of found attribute(s) :")
                ( (= mode "DYN") "\nList of found dynamic property(ies) :")
                ( (= mode "DXF") "\nList of found object(s) properties :")
              )
            )
          )
          (foreach key
            (vl-sort
              key-lst
              '(lambda (a b)
                (if (listp (car a))
                  (if (= (caar a) (caar b))
                    (< (cdar a) (cdar b))
                    (< (caar a) (caar b))
                  )
                  (< (car a) (car b))
                )
               )
            )
            (if (listp (cdr key))
              (progn
                (prompt
                  (strcat
                    "\n  - "
                    (if (listp (car key))
                      (strcat
                        (vl-prin1-to-string (caar key))
                        " = "
                        (vl-princ-to-string (cdar key))
                      )
                      (vl-prin1-to-string (car key))
                    )
                    " ("
                    (itoa (setq n (apply '+ (mapcar 'cdr (cdr key)))))
                    "u - "
                    (rtos (* 100 (/ n (atof (rtos (sslength jsel) 2 2)))) 2 2)
                    "%)"
                  )
                )
                (mapcar
                  '(lambda (e)
                    (prompt
                      (strcat
                        "\n     -> "
                        (vl-prin1-to-string (car e))
                        " ("
                        (itoa (cdr e))
                        "u - "
                        (rtos (* 100 (/ (cdr e) (atof (rtos n 2 2)))) 2 2)
                        "%)"
                      )
                    )
                   )
                  (vl-sort (cdr key) '(lambda (a b) (< (car a) (car b))))
                )
              )
              (prompt
                (strcat
                  "\n  - "
                  (vl-prin1-to-string (car key))
                  " ("
                  (itoa (setq n (cdr key)))
                  "u - "
                  (rtos (* 100 (/ n (atof (rtos (sslength jsel) 2 2)))) 2 2)
                  "%)"
                )
              )
            )
          )
          (prompt "\n")
        )
      )
    )
    (setq jsel nil)
  )
  (princ)
  (sssetfirst nil jsel)
  jsel
)