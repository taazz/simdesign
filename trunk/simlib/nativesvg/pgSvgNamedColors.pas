{ SVG named colors.

  This source code may NOT be used or replicated without prior permission
  from the abovementioned author.

  Author: Nils Haeck M.Sc.
  More information: www.simdesign.nl or n.haeck@simdesign.nl
  Copyright (c) 2008 SimDesign BV
}
unit pgSvgNamedColors;

interface

uses
  SysUtils, Pyro;

type

  TNamedColor = record
    Name:  string;
    Color: TpgColor32;
  end;

const

  // These are color names that can be used inside SVG
  cNamedColorCount = 147;
  cNamedColors: array [0..cNamedColorCount - 1] of TNamedColor =
   ((Name: 'aliceblue';            Color: $FFF0F8FF), (Name: 'antiquewhite';         Color: $FFFAEBD7),
    (Name: 'aqua';                 Color: $FF00FFFF), (Name: 'aquamarine';           Color: $FF7FFFD4),
    (Name: 'azure';                Color: $FFF0FFFF), (Name: 'beige';                Color: $FFF5F5DC),
    (Name: 'bisque';               Color: $FFFFE4C4), (Name: 'black';                Color: $FF000000),
    (Name: 'blanchedalmond';       Color: $FFFFEBCD), (Name: 'blue';                 Color: $FF0000FF),
    (Name: 'blueviolet';           Color: $FF8A2BE2), (Name: 'brown';                Color: $FFA52A2A),
    (Name: 'burlywood';            Color: $FFDEB887), (Name: 'cadetblue';            Color: $FF5F9EA0),
    (Name: 'chartreuse';           Color: $FF7FFF00), (Name: 'chocolate';            Color: $FFD2691E),
    (Name: 'coral';                Color: $FFFF7F50), (Name: 'cornflowerblue';       Color: $FF6495ED),
    (Name: 'cornsilk';             Color: $FFFFF8DC), (Name: 'crimson';              Color: $FFDC143C),
    (Name: 'cyan';                 Color: $FF00FFFF), (Name: 'darkblue';             Color: $FF00008B),
    (Name: 'darkcyan';             Color: $FF008B8B), (Name: 'darkgoldenrod';        Color: $FFB8860B),
    (Name: 'darkgray';             Color: $FFA9A9A9), (Name: 'darkgreen';            Color: $FF006400),
    (Name: 'darkgrey';             Color: $FFA9A9A9), (Name: 'darkkhaki';            Color: $FFBDB76B),
    (Name: 'darkmagenta';          Color: $FF8B008B), (Name: 'darkolivegreen';       Color: $FF556B2F),
    (Name: 'darkorange';           Color: $FFFF8C00), (Name: 'darkorchid';           Color: $FF9932CC),
    (Name: 'darkred';              Color: $FF8B0000), (Name: 'darksalmon';           Color: $FFE9967A),
    (Name: 'darkseagreen';         Color: $FF8FBC8F), (Name: 'darkslateblue';        Color: $FF483D8B),
    (Name: 'darkslategray';        Color: $FF2F4F4F), (Name: 'darkslategrey';        Color: $FF2F4F4F),
    (Name: 'darkturquoise';        Color: $FF00CED1), (Name: 'darkviolet';           Color: $FF9400D3),
    (Name: 'deeppink';             Color: $FFFF1493), (Name: 'deepskyblue';          Color: $FF00BFFF),
    (Name: 'dimgray';              Color: $FF696969), (Name: 'dimgrey';              Color: $FF696969),
    (Name: 'dodgerblue';           Color: $FF1E90FF), (Name: 'firebrick';            Color: $FFB22222),
    (Name: 'floralwhite';          Color: $FFFFFAF0), (Name: 'forestgreen';          Color: $FF228B22),
    (Name: 'fuchsia';              Color: $FFFF00FF), (Name: 'gainsboro';            Color: $FFDCDCDC),
    (Name: 'ghostwhite';           Color: $FFF8F8FF), (Name: 'gold';                 Color: $FFFFD700),
    (Name: 'goldenrod';            Color: $FFDAA520), (Name: 'gray';                 Color: $FF808080),
    (Name: 'green';                Color: $FF008000), (Name: 'greenyellow';          Color: $FFADFF2F),
    (Name: 'grey';                 Color: $FF808080), (Name: 'honeydew';             Color: $FFF0FFF0),
    (Name: 'hotpink';              Color: $FFFF69B4), (Name: 'indianred';            Color: $FFCD5C5C),
    (Name: 'indigo';               Color: $FF4B0082), (Name: 'ivory';                Color: $FFFFFFF0),
    (Name: 'khaki';                Color: $FFF0E68C), (Name: 'lavender';             Color: $FFE6E6FA),
    (Name: 'lavenderblush';        Color: $FFFFF0F5), (Name: 'lawngreen';            Color: $FF7CFC00),
    (Name: 'lemonchiffon';         Color: $FFFFFACD), (Name: 'lightblue';            Color: $FFADD8E6),
    (Name: 'lightcoral';           Color: $FFF08080), (Name: 'lightcyan';            Color: $FFE0FFFF),
    (Name: 'lightgoldenrodyellow'; Color: $FFFAFAD2), (Name: 'lightgray';            Color: $FFD3D3D3),
    (Name: 'lightgreen';           Color: $FF90EE90), (Name: 'lightgrey';            Color: $FFD3D3D3),
    (Name: 'lightpink';            Color: $FFFFB6C1), (Name: 'lightsalmon';          Color: $FFFFA07A),
    (Name: 'lightseagreen';        Color: $FF20B2AA), (Name: 'lightskyblue';         Color: $FF87CEFA),
    (Name: 'lightslategray';       Color: $FF778899), (Name: 'lightslategrey';       Color: $FF778899),
    (Name: 'lightsteelblue';       Color: $FFB0C4DE), (Name: 'lightyellow';          Color: $FFFFFFE0),
    (Name: 'lime';                 Color: $FF00FF00), (Name: 'limegreen';            Color: $FF32CD32),
    (Name: 'linen';                Color: $FFFAF0E6), (Name: 'magenta';              Color: $FFFF00FF),
    (Name: 'maroon';               Color: $FF800000), (Name: 'mediumaquamarine';     Color: $FF66CDAA),
    (Name: 'mediumblue';           Color: $FF0000CD), (Name: 'mediumorchid';         Color: $FFBA55D3),
    (Name: 'mediumpurple';         Color: $FF9370DB), (Name: 'mediumseagreen';       Color: $FF3CB371),
    (Name: 'mediumslateblue';      Color: $FF7B68EE), (Name: 'mediumspringgreen';    Color: $FF00FA9A),
    (Name: 'mediumturquoise';      Color: $FF48D1CC), (Name: 'mediumvioletred';      Color: $FFC71585),
    (Name: 'midnightblue';         Color: $FF191970), (Name: 'mintcream';            Color: $FFF5FFFA),
    (Name: 'mistyrose';            Color: $FFFFE4E1), (Name: 'moccasin';             Color: $FFFFE4B5),
    (Name: 'navajowhite';          Color: $FFFFDEAD), (Name: 'navy';                 Color: $FF000080),
    (Name: 'oldlace';              Color: $FFFDF5E6), (Name: 'olive';                Color: $FF808000),
    (Name: 'olivedrab';            Color: $FF6B8E23), (Name: 'orange';               Color: $FFFFA500),
    (Name: 'orangered';            Color: $FFFF4500), (Name: 'orchid';               Color: $FFDA70D6),
    (Name: 'palegoldenrod';        Color: $FFEEE8AA), (Name: 'palegreen';            Color: $FF98FB98),
    (Name: 'paleturquoise';        Color: $FFAFEEEE), (Name: 'palevioletred';        Color: $FFDB7093),
    (Name: 'papayawhip';           Color: $FFFFEFD5), (Name: 'peachpuff';            Color: $FFFFDAB9),
    (Name: 'peru';                 Color: $FFCD853F), (Name: 'pink';                 Color: $FFFFC0CB),
    (Name: 'plum';                 Color: $FFDDA0DD), (Name: 'powderblue';           Color: $FFB0E0E6),
    (Name: 'purple';               Color: $FF800080), (Name: 'red';                  Color: $FFFF0000),
    (Name: 'rosybrown';            Color: $FFBC8F8F), (Name: 'royalblue';            Color: $FF4169E1),
    (Name: 'saddlebrown';          Color: $FF8B4513), (Name: 'salmon';               Color: $FFFA8072),
    (Name: 'sandybrown';           Color: $FFF4A460), (Name: 'seagreen';             Color: $FF2E8B57),
    (Name: 'seashell';             Color: $FFFFF5EE), (Name: 'sienna';               Color: $FFA0522D),
    (Name: 'silver';               Color: $FFC0C0C0), (Name: 'skyblue';              Color: $FF87CEEB),
    (Name: 'slateblue';            Color: $FF6A5ACD), (Name: 'slategray';            Color: $FF708090),
    (Name: 'slategrey';            Color: $FF708090), (Name: 'snow';                 Color: $FFFFFAFA),
    (Name: 'springgreen';          Color: $FF00FF7F), (Name: 'steelblue';            Color: $FF4682B4),
    (Name: 'tan';                  Color: $FFD2B48C), (Name: 'teal';                 Color: $FF008080),
    (Name: 'thistle';              Color: $FFD8BFD8), (Name: 'tomato';               Color: $FFFF6347),
    (Name: 'turquoise';            Color: $FF40E0D0), (Name: 'violet';               Color: $FFEE82EE),
    (Name: 'wheat';                Color: $FFF5DEB3), (Name: 'white';                Color: $FFFFFFFF),
    (Name: 'whitesmoke';           Color: $FFF5F5F5), (Name: 'yellow';               Color: $FFFFFF00),
    (Name: 'yellowgreen';          Color: $FF9ACD32));

// Find the RGBA value from a named color value (named colors are defined in
// the table cNamedColors). When the color is found, the function returns True.
function pgSvgFromNamedColor32(const AName: string; var AColor: TpgColor32): boolean;

// Find the named color from the RGBA color value (named colors are defined in
// the table cNamedColors). When the color is found, the function returns True.
function pgSvgToNamedColor32(const AColor: TpgColor32; var AName: string): boolean;

implementation

function pgSvgFromNamedColor32(const AName: string; var AColor: TpgColor32): boolean;
var
  i: integer;
begin
  // Scan through list. To do: we can optimize this by using binary search
  Result := False;
  for i := 0 to cNamedColorCount - 1 do
    if AnsiCompareText(cNamedColors[i].Name, AName) = 0 then begin
      Result := True;
      AColor := cNamedColors[i].Color;
      break;
    end;
end;

function pgSvgToNamedColor32(const AColor: TpgColor32; var AName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to cNamedColorCount - 1 do
    if cNamedColors[i].Color = AColor then begin
      Result := True;
      AName := cNamedColors[i].Name;
      break;
    end;
end;

end.
