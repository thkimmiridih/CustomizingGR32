unit MainUnit;

interface

uses
  Windows, Types, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math,
  StdCtrls, ExtCtrls, GR32_Image, GR32_LowLevel,
  {$IFDEF FPC} LResources, {$ENDIF}
  GR32, GR32_Polygons, GR32_Math, GR32_Transforms, GR32_Blend,
  GR32_Layers, GR32_Misc, GR32_Text, ComCtrls;

type

  TForm1 = class(TForm)
    Panel1: TPanel;
    sbRotation: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    sbSkewX: TScrollBar;
    Label4: TLabel;
    sbSkewY: TScrollBar;
    Label5: TLabel;
    sbScaleX: TScrollBar;
    Label6: TLabel;
    sbScaleY: TScrollBar;
    Label7: TLabel;
    lblRotation: TLabel;
    lblSkewY: TLabel;
    lblScaleY: TLabel;
    lblSkewX: TLabel;
    lblScaleX: TLabel;
    Button1: TButton;
    Button2: TButton;
    FontDialog1: TFontDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Image: TImage32;
    TabSheet2: TTabSheet;
    Image2: TImage32;
    TabSheet3: TTabSheet;
    Image3: TImage32;
    TabSheet4: TTabSheet;
    Image4: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure sbRotationChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    suspendRedraw: boolean;
    procedure RedrawBitmap;
    procedure RedrawBitmapPage1;
    procedure RedrawBitmapPage2;
    procedure RedrawBitmapPage3;
    procedure RedrawBitmapPage4;
  public
    ttfc, ttfcArial16, ttfcCourNew18, ttfc30, ttfc96: TTrueTypeFont;
    fCnt: integer;
    fPts: TArrayOfFixedPoint;
  end;

var
  Form1: TForm1;

implementation

uses DateUtils;

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

{$R pattern.res}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  GR32.SetGamma(1.1); //text looks a bit thin at higher gammas

  Image.SetupBitmap;
  Image.Bitmap.DrawMode := dmOpaque;
  Image.Bitmap.CombineMode := cmBlend;
  Image.Bitmap.Clear($FFE8E8D8);

  Image2.SetupBitmap;
  Image2.Bitmap.DrawMode := dmOpaque;
  Image2.Bitmap.CombineMode := cmBlend;
  Image2.Bitmap.Clear($FFE8E8D8);

  Image3.SetupBitmap;
  Image3.Bitmap.DrawMode := dmOpaque;
  Image3.Bitmap.CombineMode := cmBlend;
  Image3.Bitmap.Clear($FFE8E8D8);

  Image4.SetupBitmap;
  Image4.Bitmap.DrawMode := dmOpaque;
  Image4.Bitmap.CombineMode := cmBlend;
  Image4.Bitmap.Clear($FFE8E8D8);

  ttfc := TrueTypeFontClass.Create(self.Font.Name, 16);
  ttfc.Hinted := true;

  ttfcArial16 := TrueTypeFontClass.Create('Arial', 16);

  ttfcCourNew18 := TrueTypeFontClass.Create('Courier New', 18, [fsBold]);
  ttfc30 := TrueTypeFontClass.Create(self.Font.Name, 30);
  ttfc96 := TrueTypeFontClass.Create(self.Font.Name, 96);

  RedrawBitmap;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ttfc.Free;
  ttfcArial16.Free;
  ttfcCourNew18.Free;
  ttfc30.Free;
  ttfc96.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  suspendRedraw := true;
  //reset transformations  ...
  sbScaleX.Position := 50;
  sbScaleY.Position := 50;
  sbSkewX.Position  := 0;
  sbSkewY.Position  := 0;
  sbRotation.Position := 0;
  sbScaleY.Enabled := (PageControl1.ActivePage = TabSheet1) or
    (PageControl1.ActivePage = TabSheet4);
  sbScaleX.Enabled := (PageControl1.ActivePage <> TabSheet3);
  sbSkewY.Enabled := (PageControl1.ActivePage <> TabSheet3);
  sbRotation.Enabled := (PageControl1.ActivePage <> TabSheet3);
  suspendRedraw := false;
  sbRotationChange(nil); //now do the redraw etc
end;
//------------------------------------------------------------------------------

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_TAB) then
    if (Shift = [ssCtrl]) then
      PageControl1.SelectNextPage(true)
    else if (Shift = [ssShift, ssCtrl]) then
      PageControl1.SelectNextPage(false);
end;
//------------------------------------------------------------------------------

procedure TForm1.Button1Click(Sender: TObject);
begin
  with FontDialog1 do
  begin
    font := self.Font;
    if not execute then exit;
    self.Font := font;

    ttfc.FontName := Font.Name;
    ttfc.Size := Font.Size;
    ttfc.Style := Font.Style;
    
    ttfc30.FontName := Font.Name;
    ttfc96.FontName := Font.Name;

    RedrawBitmap;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Button2Click(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TForm1.sbRotationChange(Sender: TObject);
begin
  if suspendRedraw then exit;
  if PageControl1.ActivePage = TabSheet4 then
    lblRotation.Caption := format('%d',[Constrain(sbRotation.Position, -45, 45)]) else
    lblRotation.Caption := format('%d',[sbRotation.Position]);
  lblSkewX.Caption   := format('%1.2f',[sbSkewX.Position/10]);
  lblSkewY.Caption   := format('%1.2f',[sbSkewY.Position/40]);
  lblScaleX.Caption   := format('%1.2f',[sbScaleX.Position/50]);
  lblScaleY.Caption   := format('%1.2f',[sbScaleY.Position/50]);
  RedrawBitmap;
end;
//------------------------------------------------------------------------------

//GradientRainbow: returns a color based on a parameter value between 0 and 1.
function GradientRainbow(fraction: single): TColor32;
begin
  if (fraction >= 1) or (fraction <= 0) then result := clRed32
  else
  begin
    fraction := fraction * 6;
    case trunc(fraction) of
      0: result := GradientColor(clRed32, clYellow32, frac(fraction));
      1: result := GradientColor(clYellow32, clLime32, frac(fraction));
      2: result := GradientColor(clLime32, clAqua32, frac(fraction));
      3: result := GradientColor(clAqua32, clBlue32, frac(fraction));
      4: result := GradientColor(clBlue32, clFuchsia32, frac(fraction));
      else result := GradientColor(clFuchsia32, clRed32, frac(fraction));
    end;
  end;
end;
//------------------------------------------------------------------------------

//SinScale: returns a value based on a sine curve where 'x' indicates the
//          relative position within the sine curve (looping at xLoop) ...
function SinScale(x: single; xLoop: integer; minScale, maxScale: single): single;
var
  dx: integer;
  diff: single;
begin
  diff := maxScale - minScale;
  if diff <= 0 then
    result := minScale
  else
  begin
    dx := round(x) mod xLoop;
    result := minScale + diff/2 + cos(dx/xLoop *2*pi)*(diff/2);
  end;
end;
//------------------------------------------------------------------------------

function LoopScale(x: single; xLoop: integer; minScale, maxScale: single): single;
var
  dx: integer;
  diff: single;
begin
  diff := maxScale - minScale;
  if (diff <= 0) or (xLoop < 1) then
    result := minScale
  else
  begin
    dx := round(x) mod xLoop;
    result := minScale + diff * dx/xLoop;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.RedrawBitmap;
begin
  if PageControl1.ActivePage = tabSheet1 then RedrawBitmapPage1
  else if PageControl1.ActivePage = tabSheet2 then RedrawBitmapPage2
  else if PageControl1.ActivePage = tabSheet3 then RedrawBitmapPage3
  else RedrawBitmapPage4;
end;
//------------------------------------------------------------------------------

procedure TForm1.RedrawBitmapPage1;
var
  i,j: integer;
  pt: TFloatPoint;
  pts: TArrayOfFixedPoint;
  polyPts: TArrayOfArrayOfFixedPoint;
  polyPolyPts: TArrayOfArrayOfArrayOfFixedPoint;
const
 txt: widestring = 'Sample text!';
begin
  caption := 'TText32 demo - ' + Self.Font.Name;
  Image.Bitmap.Clear($FFE8E8D8);
  Image.Bitmap.BeginUpdate;

  //first a bit of cosmetic fun (a broken circle around 'GR32') ...
  setlength(polyPts,4);
  polyPts[0] := GetPipeCornerPoints(FloatRect(450,0,510,60),20,cTopLeft);
  polyPts[1] := GetPipeCornerPoints(FloatRect(515,0,575,60),20,cTopRight);
  polyPts[2] := GetPipeCornerPoints(FloatRect(515,65,575,125),20,cBottomRight);
  polyPts[3] := GetPipeCornerPoints(FloatRect(450,65,510,125),20,cBottomLeft);
  SimpleShadow(Image.Bitmap, polyPts, 4, 4, MAXIMUM_SHADOW_FADE, $FFA0A0A0, true);
  SimpleGradientFill(Image.Bitmap, polyPts, $0, [$FFFFFFFF, $FFC0C0C0], -45);
  SimpleText(Image.Bitmap, panel1.Font, 496, 55, 'GR32', $FF404040);

  //3 pretty stars before we get to drawing text ...
  pts := GetStarPoints(FixedPoint(515,250),7,12,30);
  //SimpleShadow(image.Bitmap,pts,-2,-2,NO_SHADOW_FADE,$FFFFFFFF, true);
  SimpleShadow(Image.Bitmap, pts, 4, 4, MAXIMUM_SHADOW_FADE, $66000000, true);
  SimpleRadialFill(Image.Bitmap,pts,[$FFEEFF66,$00CC33CC]);
  pts := GetStarPoints(FixedPoint(548,220),5,5,10);
  //SimpleShadow(image.Bitmap,pts,-2,-2,NO_SHADOW_FADE,$FFFFFFFF, true);
  SimpleShadow(Image.Bitmap, pts, 4, 4, MAXIMUM_SHADOW_FADE, $66000000, true);
  SimpleRadialFill(Image.Bitmap,pts,[$FFEEFF66,$00CC33CC]);
  pts := GetStarPoints(FixedPoint(560,250),5,5,10);
  //SimpleShadow(image.Bitmap,pts,-2,-2,NO_SHADOW_FADE,$FFFFFFFF, true);
  SimpleShadow(Image.Bitmap, pts, 4, 4, MAXIMUM_SHADOW_FADE, $66000000, true);
  SimpleRadialFill(Image.Bitmap,pts,[$FFEEFF66,$00CC33CC]);

//  //some very basic stuff that doesn't require a TText32 object ...
//  Image.Bitmap.Canvas.Brush.Style := bsClear;
//  Image.Bitmap.Canvas.Font.Assign(self.Font);
//  //nb: the following line won't work if Image.Bitmap.DrawMode := dmBlend;
//  Image.Bitmap.Canvas.TextOut(500,150, 'TextOut');

  with TText32.Create do
  try
    //first, apply text transformations according to scrollbar sliders ...
    Scale(sbScaleX.Position/50, sbScaleY.Position/50);
    Skew(sbSkewX.Position/10, sbSkewY.Position/40);
    Angle := sbRotation.Position;

    ////////////////////////////////////////////////////////////////////////////
    // Draw a line of text using different colors, fonts and sizes            젨
    ////////////////////////////////////////////////////////////////////////////

    //nb: the first line sets the position and subsequent lines (without X & Y
    //parameters) will continue drawing at the appropriate insertion points ...
    Draw(Image.Bitmap, 20, 55, 'This is some text using different ', ttfcArial16, $FF004800);
    Draw(Image.Bitmap, 'sizes', ttfc30, $FFC36217);
    Draw(Image.Bitmap, ', ', ttfcArial16, $FF004800);
    Draw(Image.Bitmap, 'fonts', ttfcCourNew18, $FF0000FF);
    Draw(Image.Bitmap, ' and ', ttfcArial16, $FF004800);
    Draw(Image.Bitmap, 'colors', ttfcArial16, $FF00B19B);
    Draw(Image.Bitmap, '.', ttfcArial16, $FF004800);

    ////////////////////////////////////////////////////////////////////////////
    // Draw text filled and outlined ...                                      젨
    ////////////////////////////////////////////////////////////////////////////

    //nb: we're still using the same transformations as above ...

    polyPts := Get(20,170, txt, ttfc96, pt);
    //now outline 'txt' with a 1.8px pen and with a semi-transparent fill ...
    DrawAndOutline(Image.Bitmap, 20,170, txt, ttfc96, 1.8, $FF008000, $99C6EBAF);
    Simple3D(Image.Bitmap, polyPts,6,6,MAXIMUM_SHADOW_FADE,clWhite32,clBlack32);

    ////////////////////////////////////////////////////////////////////////////
    // Draw text along a bezier path ...                                       ?
    ////////////////////////////////////////////////////////////////////////////

    //ignore rotation and skew ...
    ClearTransformations;
    Scale(sbScaleX.Position/100, sbScaleY.Position/100);
    Skew(sbSkewX.Position/10, 0);

    //create a bezier path on which to locate some text ...
    pts := GetCBezierPoints([FixedPoint(20,260),FixedPoint(170,180),
        FixedPoint(170,340),FixedPoint(320,260)]);
    SimpleLine(Image.Bitmap, pts, $40009900); //draw the path

    //draw the text centered on this path...
    //This is a bit more sophisticated than what a Draw method offers since
    //rather than simply drawing, we get the points to apply custom coloring etc.
    //Also, we'll offset the text a couple of pixels away from the line ...
    polyPolyPts := GetEx(pts, txt, ttfc96, aCenter, aMiddle, true, 2);
    for i := 0 to high(polyPolyPts) do
    begin
      SimpleShadow(image.Bitmap,
        polyPolyPts[i],-3,-3,MINIMUM_SHADOW_FADE, clWhite32, true, true);
      SimpleShadow(image.Bitmap,
        polyPolyPts[i],3,3,MAXIMUM_SHADOW_FADE, $80000000, true, true);
      if length(polyPolyPts[i]) > 0 then
      begin
        j := FixedRound(polyPolyPts[i][0][0].X);
        SimpleFill(image.Bitmap, polyPolyPts[i],
          $20000000, GradientRainbow(LoopScale(j, 1100, 0, 1)));
      end;
    end;

  finally
    free;
  end;
  Image.Bitmap.EndUpdate;
end;
//------------------------------------------------------------------------------

procedure TForm1.RedrawBitmapPage2;
var
  i: integer;
  s: single;
  clr: TColor32;
  pts: TArrayOfFixedPoint;
  polyPts, inflatedPolyPts: TArrayOfArrayOfFixedPoint;
  pos, nextpos: TFloatPoint;
const
  txt: wideString = 'FUN WITH GRAPHICS32';

  procedure DoFill(const pts: TArrayOfFixedPoint; clrs: array of TColor32);
  begin
    SimpleShadow(image2.Bitmap,pts,3,3,MAXIMUM_SHADOW_FADE, clrs[2], true);
    SimpleLine(Image2.Bitmap, pts, clrs[2]);
    SimpleGradientFill(Image2.Bitmap,pts,$0,clrs,-90);
  end;

begin

  caption := 'TText32 demo - ' + Self.Font.Name;
  Image2.Bitmap.Clear($FFE8E8D8);
  Image2.Bitmap.BeginUpdate;
  pts := nil;

////////////////////////////////////////////////////////////////////////////////
// something a bit more complicated ...                                        ?
////////////////////////////////////////////////////////////////////////////////

  //first, just a couple of pretty lines ...
  pts := MakeArrayOfFixedPoints([20,30, 550,30, 550,36, 20,36]);
  DoFill(pts, [$FF6666FF,$FFAAAAFF,$FF000033]);
  pts := MakeArrayOfFixedPoints([30,26, 40,26, 40,40, 30,40]);
  DoFill(pts, [$FF6666FF,$FFAAAAFF,$FF000033]);
  pts := MakeArrayOfFixedPoints([530,26, 540,26, 540,40, 530,40]);
  DoFill(pts, [$FF6666FF,$FFAAAAFF,$FF000033]);

  pts := MakeArrayOfFixedPoints([20,250, 550,250, 550,256, 20,256]);
  DoFill(pts, [$FFAA66AA,$FFFF99FF,$FF330033]);
  pts := MakeArrayOfFixedPoints([30,246, 40,246, 40,260, 30,260]);
  DoFill(pts, [$FFAA66AA,$FFFF99FF,$FF330033]);
  pts := MakeArrayOfFixedPoints([530,246, 540,246, 540,260, 530,260]);
  DoFill(pts, [$FFAA66AA,$FFFF99FF,$FF330033]);

  //This only looks good with Option 1 (see below), otherwise leave commented out.
//  SimpleGradientFill(Image2.Bitmap,
//    [FixedPoint(10,70),FixedPoint(560,70),FixedPoint(560,220),FixedPoint(10,220)],
//    $0, [$FFF4B651, $FFD75F00], -90);

  with TText32.Create do
  try
    Angle := sbRotation.Position;
    Skew(sbSkewX.Position/10, sbSkewY.Position/40);
    Spacing := 0;

    pos := FloatPoint(20, 170);
    for i := 1 to length(txt) do
    begin
      //scale and translate depending the position of the char in the line ...
      s := SinScale(pos.X, 330, 1, 2.5); //ie makes 's' between 1 and 2.5
      Scale(0.5* sbScaleX.Position/50, 0.6* s);
      //make the taller (larger Y scaled) characters start a little lower down
      //so all the characters are aligned roughly through their middles ...
      Translate(0, SinScale(pos.X, 330, 0, 18));

      //get the polypoints for each individual character ...
      polyPts := Get(pos.X, pos.Y, txt[i], ttfc96, nextpos);
      inflatedPolyPts := GetInflated(pos.X, pos.Y, 2.0, txt[i], ttfc96, nextpos);
      OffsetPolyPoints(inflatedPolyPts, -1,-1);
      pos := nextpos;

      //Option 1.
//      SimpleShadow(Image2.Bitmap, inflatedPolyPts, 4, 4, MEDIUM_SHADOW_FADE, $80000000, true);
//      SimpleGradientFill(Image2.Bitmap, inflatedPolyPts, $0, [clWhite32, $FFC0C0C0], -90);

      //Option 2.
//      SimpleShadow(Image2.Bitmap, inflatedPolyPts, 4, 4, MEDIUM_SHADOW_FADE, $80000000, true);
//      SimpleFill(Image2.Bitmap, inflatedPolyPts, $0, clWhite32);
//      clr := GradientRainbow(LoopScale(pos.X, 500, 0, 1));
//      SimpleFill(Image2.Bitmap, polyPts, $0, clr);

      //Option 3.
      SimpleShadow(Image2.Bitmap, inflatedPolyPts, 4, 4, MEDIUM_SHADOW_FADE, $80000000, true);
      SimpleFill(Image2.Bitmap, inflatedPolyPts, $0, clWhite32);
      clr := GradientRainbow(LoopScale(pos.X, 500, 0, 1));
      SimpleGradientFill(Image2.Bitmap, polyPts, $0,
        [GradientColor(clr,clBlack32,0.25), clr, clr, clr, SetAlpha(clr, 64)],115);
    end;

  finally
    free;
  end;
  Image2.Bitmap.EndUpdate;
end;
//------------------------------------------------------------------------------

procedure TForm1.RedrawBitmapPage3;
var
  pts, upperPts, lowerPts: TArrayOfFixedPoint;
  ppFx: TArrayOfArrayOfFixedPoint;
  ppFt: TArrayOfArrayOfFloatPoint;
  bmp: TBitmap32;
const
  txt: wideString = 'Text between paths ...';
begin

  caption := 'TText32 demo - ' + Self.Font.Name;
  Image3.Bitmap.Clear($FFE8E8D8);
  Image3.Bitmap.BeginUpdate;
  Image3.Bitmap.DrawMode := dmBlend;
  Image3.Bitmap.CombineMode := cmBlend;

////////////////////////////////////////////////////////////////////////////////
// Text between 2 straight lines ...                                           ?
////////////////////////////////////////////////////////////////////////////////

  upperPts := MakeArrayOfFixedPoints([10,50,  110,20]);
  lowerPts := MakeArrayOfFixedPoints([10,100,  110,130]);
  //first, draw a subtle shade between these 2 lines ...
  pts := MakeArrayOfFixedPoints([10,50,  110,20, 110,130, 10,100]);
  SimpleGradientFill(Image3.Bitmap, Pts, $0, [$80E0F8E0,$80FFFFD0,$00FFFFD0],0);
  //next, draw the lines ...
  SimpleLine(Image3.Bitmap, upperPts, $40999900);
  SimpleLine(Image3.Bitmap, lowerPts, $40999900);
  //get the text outline points ...
  with TText32.Create do
  try
    Skew(sbSkewX.Position/10, 0);
    ppFx := GetBetweenPaths(lowerPts,upperPts,'GR32', ttfc96);
  finally
    free;
  end;

  //create a 3D effect for the text ...
  SimpleShadow(Image3.Bitmap,ppFx, -3, -3, NO_SHADOW_FADE, $FFFFFFFF, true);
  SimpleShadow(Image3.Bitmap,ppFx, 2, 2, NO_SHADOW_FADE, $88333300, true);

  //finally, fill the text with a green, yellow and red color gradient ...
  SimpleGradientFill(Image3.Bitmap, ppFx, $0,
    [$80FF0000,$80FF0000,$80FFFF00,$FFFFFF00,$FF00FF00],115);

////////////////////////////////////////////////////////////////////////////////
// Text between 2 very wavy curves ...                                         ?
////////////////////////////////////////////////////////////////////////////////

  upperPts := GetCBezierPoints([FixedPoint(170,40),FixedPoint(270,-60),
      FixedPoint(270,140),FixedPoint(370,40),
      FixedPoint(470,-60),FixedPoint(470,140),FixedPoint(570,40)]);
  lowerPts := GetCBezierPoints([FixedPoint(170,110),FixedPoint(270,10),
      FixedPoint(270,210),FixedPoint(370,110),
      FixedPoint(470,10),FixedPoint(470,210),FixedPoint(570,110)]);

  SimpleLine(Image3.Bitmap, upperPts, $400000AA);
  SimpleLine(Image3.Bitmap, lowerPts, $400000AA);

  with TText32.Create do
  try
    Spacing := 10;
    Skew(sbSkewX.Position/10, 0);
    ppFx := GetBetweenPaths(lowerPts,upperPts,txt,ttfc96);
  finally
    free;
  end;                     

  //3D drop shadow effect ...
  SimpleShadow(Image3.Bitmap,ppFx, 4, 4, MINIMUM_SHADOW_FADE, $FF330066, true);

  //draw a 3px wide text outline (will be partly obscured by following pattern)
  ppFt := MakeArrayOfArrayOfFloatPoints(ppFx);
  PolyPolylineFS(image3.Bitmap, ppFt, $FF330066, true, 3);

  //finally fill the text with a bitmap pattern ...
  bmp := TBitmap32.Create;
  try
    bmp.LoadFromResourceName(hInstance, 'PATTERN2');
    SimpleFill(Image3.Bitmap, ppFx, $0, bmp);
  finally
    bmp.Free;
  end;

////////////////////////////////////////////////////////////////////////////////
// Text in a triangle (but still between 2 paths) ...                          ?
////////////////////////////////////////////////////////////////////////////////

  //some fancy text in a triangle ...
  upperPts := MakeArrayOfFixedPoints([10,280,  160,110,  310,280]);
  lowerPts := MakeArrayOfFixedPoints([10,280,           310,280]);

  //shade the triangle with a very subtle radial gradient ...
  SimpleRadialFill(Image3.Bitmap,upperPts,[$30C0C020,$20C08020,$00FF8080]);
  SimpleLine(Image3.Bitmap, upperPts, $40FF0000);
  SimpleLine(Image3.Bitmap, lowerPts, $40FF0000);

  //get the text outline ...
  with TText32.Create do
  try
    Skew(sbSkewX.Position/10, 0);
    ppFx := GetBetweenPaths(lowerPts,upperPts,'  Graphics  ',ttfc96);
  finally
    free;
  end;

  //3D effects ...
  SimpleShadow(Image3.Bitmap,ppFx,-3,-3,NO_SHADOW_FADE, clWhite32, True);
  SimpleShadow(Image3.Bitmap,ppFx, 2, 2,NO_SHADOW_FADE, $AA333300, True);

  SimpleRadialFill(Image3.Bitmap,ppFx,[$FFF0F080,$FFFF8080]);
////////////////////////////////////////////////////////////////////////////////
// Text in a circle (but still between 2 paths) ...                            ?
////////////////////////////////////////////////////////////////////////////////

  pts := GetEllipsePoints(FloatRect(327,107,513,293));
  SimpleRadialFill(Image3.Bitmap,pts,
    [$60FFFF66,$40FFFF66,$80AAFF66,$60AAFF66,$0060AA80]);

  //divide the ellipse into a top half and bottom half
  upperPts := GetArcPoints(FloatRect(330,110,510,290),0,180);
  //GetArcPoints returns the arc in an anticlockwise direction so it's important
  //to make the curve (like the text) go from left to right ...
  upperPts := ReversePoints(upperPts);
  //lowerPts don't need reversing ...
  lowerPts := GetArcPoints(FloatRect(330,110,510,290),180,0);

  SimpleLine(Image3.Bitmap, upperPts, $8088FF66);
  SimpleLine(Image3.Bitmap, lowerPts, $8088FF66);

  //now get the text outline points ...
  with TText32.Create do
  try
    Skew(sbSkewX.Position/10, 0);
    ppFx := GetBetweenPaths(lowerPts,upperPts,'  Delphi  ',ttfc96);
  finally
    free;
  end;

  //3D effects ...
  SimpleShadow(Image3.Bitmap,ppFx,-3,-3,NO_SHADOW_FADE, clWhite32, True);
  SimpleShadow(Image3.Bitmap,ppFx, 2, 2,NO_SHADOW_FADE, $AA333300, True);

  bmp := TBitmap32.Create;
  try
    bmp.LoadFromResourceName(hInstance, 'PATTERN');
    SimpleFill(Image3.Bitmap, ppFx, $0, bmp);
  finally
    bmp.Free;
  end;
  Image3.Bitmap.EndUpdate;
end;
//------------------------------------------------------------------------------

procedure TForm1.RedrawBitmapPage4;
var
  chrCnt,len,linePos,textArrayPos,chrPos: integer;
  lineHeight: single;
  pt, startPt: TFloatPoint;
  currentText, currentLine: UnicodeString;
  currentColor: TColor32;
  rec1, rec2: TFloatRect;
  pts: TArrayOfFixedPoint;
  text32: TText32;
  tm: TTextMetric;
const
  txt1: UnicodeString = 'This text is horizontally justified and vertically '+
  'centered with a line break at a LineFeed (#10) character.'#10+
  'Alignment is also preserved when text is skewed or scaled.';

  txt2: UnicodeString = 'This text is centered horizontally and vertically.';

  txt3: array [0..6] of UnicodeString = (
  'With a small amount of effort it''s possible to display ',
  'formatted text. ',
  'This example demonstrates left justified text with simple ',
  'text coloring',
  '. However it wouldn''t require too much more work to add in ',
  'text styles ',
  'too.');

  procedure GetCurrentColor(arrayPos: integer);
  begin
    case arrayPos of
      1: currentColor := clRed32;
      3: currentColor := clBlue32;
      5: currentColor := clGreen32;
      else currentColor := clBlack32;
    end;
  end;

begin
  caption := 'TText32 demo - ' + Self.Font.Name;
  Image4.Bitmap.Clear($FFE8E8D8);
  Image4.Bitmap.BeginUpdate;

////////////////////////////////////////////////////////////////////////////////
// Justified and centered text in bounding rectangles ...                      ?
////////////////////////////////////////////////////////////////////////////////

  //define and draw bounding rectangle 1 ...
  rec1 := FloatRect(20, 20, 290, 165);
  pts := MakeArrayOfFixedPoints(rec1);
  SimpleShadow(Image4.Bitmap, pts, 5, 5, MAXIMUM_SHADOW_FADE, $FF000000, true);
  SimpleFill(Image4.Bitmap, pts, $80000000, $FFFFFFFF);
  InflateRect(rec1,-10,-10);
  SimpleLine(Image4.Bitmap, MakeArrayOfFixedPoints(rec1), $20000000, true);

  //define and draw bounding rectangle 2 ...
  rec2 := FloatRect(45, 175, 265, 280);
  pts := GetEllipsePoints(rec2);
  SimpleShadow(Image4.Bitmap, pts, 5, 5, MAXIMUM_SHADOW_FADE, $FF000000, true);
  SimpleFill(Image4.Bitmap, pts, $80000000, $FFFFFFFF);
  InflateRect(rec2,-55,-8);
  //SimpleLine(Image4.Bitmap, MakeArrayOfFixedPoints(rec2), $20000000, true);

  //now draw text within the two bounding rectangles ...
  text32 := TText32.Create;
  try
    text32.Scale(sbScaleX.Position/50, sbScaleY.Position/50);
    text32.Skew(sbSkewX.Position/10, 0);
    text32.draw(Image4.Bitmap, rec1, txt1, ttfc, clBlack32, aJustify, aMiddle, true);
    text32.draw(Image4.Bitmap, rec2, txt2, ttfc, clBlack32, aCenter, aMiddle, true);
  finally
    text32.Free;
  end;

////////////////////////////////////////////////////////////////////////////////
// Left aligned text with color highlighting ...                               ?
////////////////////////////////////////////////////////////////////////////////

  //define and draw a new bounding rectangle ...
  rec1 := FloatRect(300, 20, Image4.ClientWidth -20, Image4.ClientHeight -20);
  pts := MakeArrayOfFixedPoints(rec1);
  SimpleShadow(Image4.Bitmap, pts, 5, 5, MAXIMUM_SHADOW_FADE, $FF000000, true);
  SimpleFill(Image4.Bitmap, pts, $80000000, $FFFFFFFF);
  InflateRect(rec1,-10,-10);
  SimpleLine(Image4.Bitmap, MakeArrayOfFixedPoints(rec1), $20000000, true);

  text32 := TText32.Create;
  try
    text32.Scale(sbScaleX.Position/50, sbScaleY.Position/50);
    text32.Skew(sbSkewX.Position/10, sbSkewY.Position/40);
    text32.Angle := Constrain(sbRotation.Position, -45, 45);

    linePos := 0;
    textArrayPos := 0;
    ttfc.GetTextMetrics(tm);
    lineHeight := round(tm.tmHeight * text32.ScaleY *6/5);
    //set starting point for text drawing ...
    startPt := FloatPoint(rec1.Left, rec1.Top + lineHeight);

    pt := startPt;
    while textArrayPos < length(txt3) do
    begin
      //nb: while we're starting a new string in txt3[] here,
      //it's very likely we're *not* at the start of a new line.
      GetCurrentColor(textArrayPos);
      currentText := txt3[textArrayPos];
      inc(textArrayPos);
      len := length(currentText);
      chrPos := 1;
      while chrPos <= len do
      begin
        currentLine := copy(currentText, chrPos, MaxInt);
        //Because the first char in each line is flush up against the left side
        //of the bounding rectangle, it's possible for the glyph to partially
        //overlap that boundary (especially when the text is skewed or rotated).
        //Therefore if we're at the start of a line, it's important to adjust
        //the X offset (and rounding just minimizes any character blurring) ...
        if pt.X = startPt.X then
          pt.X := round(pt.X - text32.GetTextFixedRect(0,0,
            currentLine[1], ttfc).Left *FixedToFloat);

        //now trim currentLine so that it fits inside rec ...
        chrCnt := text32.CountCharsThatFit(pt.X, pt.Y, currentLine, ttfc, rec1, true);
        if (chrCnt < length(currentLine))
          and (currentLine[chrCnt+1] = ' ') then inc(chrCnt);
        currentLine := copy(currentLine, 1, chrCnt);
        text32.Draw(Image4.Bitmap, pt.X, pt.Y, currentLine, ttfc, currentColor);
        //finally, get ready for the next loop ...
        inc(chrPos, chrCnt);
        if chrPos > len then break;
        inc(linePos);
        pt.X := startPt.X;
        pt.Y := startPt.Y + linePos*lineHeight;
        if chrPos > len then
          text32.CurrentPos := FixedPoint(pt);
        //and make sure we haven't reached the bottom ...
        if pt.Y > rec1.Bottom then
        begin
          textArrayPos := MaxInt; //ie force a complete break
          break;
        end;
      end;
      //make sure the next string in txt3[] starts drawing at the right place...
      pt := FloatPoint(text32.CurrentPos);
    end;
  finally
    text32.Free;
  end;

  Image4.Bitmap.EndUpdate;
end;
//------------------------------------------------------------------------------


end.
