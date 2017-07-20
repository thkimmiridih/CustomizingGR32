unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is the PolyLine Example code.
 *
 * The Initial Developer of the Original Code is Angus Johnson
 *
 * The Original Code is Copyright (C) 2008-9. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$IFDEF FPC} LResources, {$ENDIF}
  GR32, GR32_Image, GR32_Misc, GR32_Lines, GR32_Polygons, Math, ExtCtrls,
  GR32_Transforms,GR32_Layers, ComCtrls, Messages;

type
  TBtnState = (bsDefault, bsHighlight, bsPressed);

  TForm1 = class(TForm)
    Image: TImage32;
    ScrollBar1: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    btnState: TBtnState;
    CloseBtn: TLine32;
    CloseBtnPts: TArrayOfFixedPoint;
    CloseBtnPts2: TArrayOfFixedPoint;
    procedure BuildImage;
    procedure OnShortCut(var Message: TWMKey; var Handled: boolean);
  public
  end;

var
  Form1: TForm1;
  clCaptionActive32, clCaptionActiveGrad32: TColor32;
  clBtnShadow32, clBtnHighlight32, clBtnFace32: TColor32;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

{$R PATTERN.RES}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Gr32.SetGamma(1.0);
  Image.SetupBitmap;

  //setup the CloseBtn line/shape object (which will be drawn in BuildImage)
  CloseBtn := TLine32.Create;
  CloseBtn.EndStyle := esClosed;
  CloseBtn.SetPoints(GetEllipsePoints(FloatRect(415, 337, 505, 373)));
  CloseBtnPts := CloseBtn.Points;
  CloseBtnPts2 := CloseBtn.GetOuterEdge(3);
  Application.OnShortCut := OnShortCut;
  Application.Title := 'TLine32 Demo';
  clCaptionActive32 := Color32(clActiveCaption);
  clCaptionActiveGrad32 := Color32(clGradientActiveCaption);
  clBtnShadow32 := Color32(clBtnShadow);
  clBtnHighlight32 := Color32(clBtnHighlight);
  clBtnFace32 := Color32(clBtnFace);

  Image.Bitmap.Canvas.Font.Assign(self.Font);
  Image.Bitmap.Canvas.Brush.Style := bsClear;

  BuildImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseBtn.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.OnShortCut(var Message: TWMKey; var Handled: boolean);

  procedure AnimatedClose;
  begin
    handled := true;
    btnState := bsPressed;
    BuildImage;
    sleep(200);
    btnState := bsDefault;
    BuildImage;
    sleep(200);
    close;
  end;

begin
  //close on ESC and Alt+C ...
  case Message.CharCode of
    VK_ESCAPE: AnimatedClose;
    Ord('C'): if (GetKeyState(VK_MENU) < 0) then AnimatedClose;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  //check if it's the CloseBtn being pressed ...
  if PtInPolygon(FixedPoint(X,Y), CloseBtn.GetOuterEdge) then
  begin
    btnState := bsPressed;
    BuildImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
var
  oldBtnState: TBtnState;
begin
  //update the CloseBtn state and redraw if necessary ...
  if btnState = bsPressed then exit;
  oldBtnState := btnState;
  if PtInPolygon(FixedPoint(X,Y), CloseBtn.GetOuterEdge) then
  begin
    screen.Cursor := crHandPoint;
    btnState := bsHighlight;
    if btnState <> oldBtnState then BuildImage;
  end else
  begin
    screen.Cursor := crDefault;
    btnState := bsDefault;
    if btnState <> oldBtnState then BuildImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

  procedure AnimatedClose;
  begin
    btnState := bsPressed;
    BuildImage;
    sleep(200);
    btnState := bsDefault;
    BuildImage;
    sleep(200);
    close;
  end;

begin
  if btnState <> bsPressed then exit;
  //update the CloseBtn state and redraw if necessary ...
  if PtInPolygon(FixedPoint(X,Y), CloseBtn.GetOuterEdge) then
  begin
    btnState := bsHighlight;
    AnimatedClose;
  end else
  begin
    btnState := bsDefault;
    BuildImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  BuildImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.BuildImage;
var
  bmp: TBitmap32;
  pts, outerPts, innerPts: TArrayOfFixedPoint;
  rec, closeBtnTextRec: TRect;
  dashes: TArrayOfFloat;
begin
  Image.Bitmap.BeginUpdate;
  Image.Bitmap.Clear($FFF3F3E4);
  Image.Bitmap.StippleStep := ScrollBar1.Position * 0.01;
  Image.Bitmap.Canvas.Font.Color := $330033;

  with TLine32.Create do
  try
////////////////////////////////////////////////////////////////////////////////
// Rectangle with rounded corners (varying with rotation) ...
////////////////////////////////////////////////////////////////////////////////
    EndStyle := esClosed;
    SetPoints(
      GetRoundedRectanglePoints(
        FloatRect(57, 57, 343, 343),
        ScrollBar1.Position div 2)); //amount of rounding (but between 5 & 90)

    //various transformation options...
    //Scale(ScrollBar1.Position/50,ScrollBar1.Position/50);
    //Translate(-ScrollBar1.Position+50,-ScrollBar1.Position+50);
    Rotate(FloatPoint(200,200), -ScrollBar1.Position/100* pi);

    //SimpleFill() is not a method of TLine32 (because it's not really
    //a line drawing operation) but is a helper function that creates a
    //TPolygon32 object, adds the array of points (in this case TLine32.Points)
    //and fills the polygon with the specified colors ...
    SimpleFill(Image.Bitmap, Points, $00000000, $100033FF);
    //draw the polyline (ie rounded rectangle) with a stippling pattern ...
    Draw(image.Bitmap, 3, [$800000FF, $800000FF, $800000FF, $800000FF,
      $800000FF, $40ECE9D8, $40ECE9D8]);
    //Draw(3, $800000FF);

////////////////////////////////////////////////////////////////////////////////
// Single straight line with rounded ends ...
////////////////////////////////////////////////////////////////////////////////
    EndStyle := esRounded;
    SetPoints([FixedPoint(30,90), FixedPoint(370,90)]);
    Draw(image.Bitmap, 7, $99800040);

////////////////////////////////////////////////////////////////////////////////
// Fancy diamond (with horizontal color gradient fill and also outlined)...
////////////////////////////////////////////////////////////////////////////////
    EndStyle := esClosed;
    JoinStyle := jsMitered;
    //set the 4 points of the diamond shape ...
    SetPoints([
      FixedPoint(35,200), FixedPoint(200,35),
      FixedPoint(365,200), FixedPoint(200,365)]);
    //gradient fill a 9px wide polyline (diamond) ...
    DrawGradient(image.Bitmap, 9, [$CCFF6600,$CCFFFF00], 115);

    //now draw the outer edges of the diamond shape Black ...
    //nb: if Line32 object wasn't 'closed' getOuterEdge would return outline
    SetPoints(GetOuterEdge);
    Draw(image.Bitmap, 1, clBlack32);
    //reset the original (diamond) points before getting another outline
    //otherwise we'd get an outline of an outline!
    SetPoints([
      FixedPoint(35,200), FixedPoint(200,35),
      FixedPoint(365,200), FixedPoint(200,365)]);
    //nb: specify the previous 9px linewidth, since last Draw set linewidth = 1
    SetPoints(GetInnerEdge(9));
    //fill the interior of the diamond shape, also drawing an edge which
    //corresponds to the inner edge of the diamond ...
    SimpleRadialFill(Image.Bitmap,Points,[$EEFFFF00,$2033FFFF]);
    Draw(image.Bitmap, 1, $FF990000);
    //gr32_lines.SimpleFill(Image.Bitmap, Points, $FF990000, $eeECE9D8);

////////////////////////////////////////////////////////////////////////////////
// Single line gradient filled and with an 'arrow' on each end ...
////////////////////////////////////////////////////////////////////////////////

    EndStyle := esSquared;
    ArrowStart.Style := asDiamond;
    ArrowStart.Size := 25;
    ArrowStart.Color := $30AA00AA;
    ArrowStart.Pen.Width := 2;
    ArrowStart.Pen.Color := $FF800040;

    ArrowEnd.Style := asSquare;
    ArrowEnd.Size := 12;
    ArrowEnd.Color := $30CC8000;
    ArrowEnd.Pen.Width := 2;
    ArrowEnd.Pen.Color := $FFAA6600;

    SetPoints([FixedPoint(30,310), FixedPoint(380,310)]);
    DrawGradient(image.Bitmap, 3, [$FF800040,$FFCC8000], 0);

    //now disable arrows ...
    ArrowStart.Style := asNone;
    ArrowEnd.Style := asNone;

////////////////////////////////////////////////////////////////////////////////
// Dashed circle ...
////////////////////////////////////////////////////////////////////////////////

    //dash, dot, dot, dash, dot, dot ... pattern ...
    dashes := MakeArrayOfFloat([18, 5, 5, 5, 5, 5]);
    pts := GetEllipsePoints(FloatRect(155, 155, 245, 245));
    SetPoints(pts);
    EndStyle := esClosed;
    Draw(image.Bitmap, 10, dashes, $40ffaa00, clBlack32);

////////////////////////////////////////////////////////////////////////////////
// Small triangle ...
////////////////////////////////////////////////////////////////////////////////
    EndStyle := esClosed;

    SetPoints([FixedPoint(90,190),
      FixedPoint(200,110), FixedPoint(310,190)]);
    //add a subtle 3D shadow effect ...
    pts := GetInnerEdge(9);
    SimpleShadow(Image.Bitmap,pts,5,5,MAXIMUM_SHADOW_FADE,$FF336633, true);
    pts := GetOuterEdge(9);
    SimpleShadow(Image.Bitmap,pts,5,5,MAXIMUM_SHADOW_FADE,$FF336633, true);

    Draw(image.Bitmap, 9, $CC00FF00, $FF003300);

////////////////////////////////////////////////////////////////////////////////
// Inverted small triangle ...
////////////////////////////////////////////////////////////////////////////////
    JoinStyle := jsRounded;
    SetPoints([FixedPoint(90,210),
      FixedPoint(200,290), FixedPoint(310,210)]);
    //add a subtle 3D shadow effect ...
    pts := GetInnerEdge(9);
    SimpleShadow(Image.Bitmap,pts,5,5,MAXIMUM_SHADOW_FADE,$FF336633, true);
    pts := GetOuterEdge(9);
    SimpleShadow(Image.Bitmap,pts,5,5,MAXIMUM_SHADOW_FADE,$FF336633, true);

    Draw(image.Bitmap, 9, $CC00FF00, $FF003300);
    //Draw(image.Bitmap, 9, [$CC00FF00, $CC00FF00, $CC00FF00, $FFECE9D8, $FFECE9D8]);
    //DrawGradient(image.Bitmap, 9, [$FF00CCCC,$FF00FF00], 135);

////////////////////////////////////////////////////////////////////////////////
// Bezier Curve (rotating) ...
////////////////////////////////////////////////////////////////////////////////
    EndStyle := esRounded;
    JoinStyle := jsMitered;
    SetPoints(
      GetCBezierPoints([FixedPoint(320,190),
        FixedPoint(200,50), FixedPoint(200,350), FixedPoint(80,210)]));

    Rotate(FloatPoint(200,200), ScrollBar1.Position/100 *pi + pi/2);
    Draw(image.Bitmap, 9, $20000066); //v. translucent blue (but looks gray due to background)

    //now draw a stippled outline of this bezier ...
    SetPoints(GetOutline);
    Draw(image.Bitmap, 2, [$FF666666, $FF666666, $FF666666, $00000000, $00000000]);

////////////////////////////////////////////////////////////////////////////////
// Ellipse (circle) with a pattern fill and fancy outlines ...
////////////////////////////////////////////////////////////////////////////////
    EndStyle := esClosed;
    pts := GetEllipsePoints(FloatRect(60,60,340,340));
    SetPoints(pts);
    outerPts := GetOuterEdge(12);
    innerPts := GetInnerEdge(9);

    //load a bitmap pattern and draw a 9px wide line (circle) ...
    bmp := TBitmap32.Create;
    try
      bmp.LoadFromResourceName(hInstance, 'PATTERN2');
      Draw(image.Bitmap, 9, bmp);
    finally
      bmp.Free;
    end;

    SetPoints(outerPts);
    Draw(Image.Bitmap,2.5,$FF003366);
    SetPoints(innerPts);
    Draw(Image.Bitmap,1.5,$FF003388);

////////////////////////////////////////////////////////////////////////////////
// Lots of arrowed lines with text ...
////////////////////////////////////////////////////////////////////////////////

    //setup the basics ...
    EndStyle := esSquared;
    ArrowStart.Pen.Width := 1.5;
    ArrowEnd.Pen.Width := 1.5;
    ArrowEnd.Size := 15;
    ArrowStart.Style := asCircle;
    ArrowStart.Size := 5;

    ///////////////////////////////////////////////////
    ArrowStart.Color := $80FFCC00;
    ArrowEnd.Color := $80FFCC00;
    ArrowStart.Pen.Color := $FF662200;
    ArrowEnd.Pen.Color := $FF662200;
    ArrowEnd.Style := asFourPoint;
    pts := GetCBezierPoints([FixedPoint(415,125), FixedPoint(350,125),
      FixedPoint(385,55), FixedPoint(425,110)]);
    SetPoints(pts);
    Draw(image.Bitmap, 2, $FF993300);

    //some text (using the canvas so there's optimal rendering) ...
    Image.Bitmap.Canvas.TextOut(422, 115,'Arrow ends');

    ///////////////////////////////////////////////////
    ArrowStart.Color := $CC00CCCC;
    ArrowStart.Pen.Color := $FF333399;
    ArrowEnd.Color := $AA00E0E0;
    ArrowEnd.Pen.Color := $FF333399;
    ArrowEnd.Size := 14;
    pts := GetQBezierPoints([
      FixedPoint(415,155), FixedPoint(200,200),
      RotatePoint(FixedPoint(80,80), FixedPoint(200,200),
      -ScrollBar1.Position/100* pi)]);
    SetPoints(pts);
    //Draw(image.Bitmap, 2, $FF333399);
    Draw(image.Bitmap, 2.5, [$FF333399,$FF333399,$00000000, $00000000, $00000000],0.65);
    Image.Bitmap.Canvas.TextOut(422,145,'Rounded rectangles');

    ///////////////////////////////////////////////////
    ArrowStart.Color := $8000FF33;
    ArrowStart.Pen.Color := $FF004800;
    ArrowEnd.Color := $FF00FF33;
    ArrowEnd.Pen.Color := $FF004800;
    ArrowEnd.Size := 12;
    ArrowEnd.Pen.Width := 2;
    SetPoints([FixedPoint(415,200),FixedPoint(324,190)]);
    Draw(image.Bitmap, 2, $FF004800);
    SetPoints([FixedPoint(415,200), FixedPoint(325,210)]);
    Draw(image.Bitmap, 2, $FF004800);
    Image.Bitmap.Canvas.TextOut(422,185,'Bevelled and');
    Image.Bitmap.Canvas.TextOut(422,200,'rounded line joins');

    ///////////////////////////////////////////////////
    ArrowStart.Color := $CC00CCCC;
    ArrowStart.Pen.Color := $FF333399;
    ArrowEnd.Color := $AA00E0E0;
    ArrowEnd.Pen.Width := 2;
    ArrowEnd.Pen.Color := $FF333399;
    ArrowEnd.Style := asThreePoint;
    ArrowEnd.Size := 14;

    SetPoints([FixedPoint(415,255),FixedPoint(345,255)]);
    Draw(image.Bitmap, 2, $FF333399);
    Image.Bitmap.Canvas.TextOut(422,230,'Lines with pattern fills');
    Image.Bitmap.Canvas.TextOut(422,245,'and lines outlining');
    Image.Bitmap.Canvas.TextOut(422,260,'other lines');

    ///////////////////////////////////////////////////
    ArrowStart.Color := $80FFCC00;
    ArrowStart.Pen.Color := $FF662200;
    ArrowEnd.Color := $FFFFCC00;
    ArrowEnd.Pen.Color := $FF662200;
    ArrowEnd.Size := 12;
    ArrowEnd.Pen.Width := 1.5;
    ArrowEnd.Style := asFourPoint;

    pts := GetQBezierPoints([FixedPoint(415,300),
    FixedPoint(380,270),FixedPoint(345,305)]);
    SetPoints(pts);
    Draw(image.Bitmap, 2, [$FF662200, $FF662200, $00000000, $00000000, $00000000],0.65);

    pts := GetCBezierPoints([FixedPoint(415,300),
    FixedPoint(385,270),FixedPoint(315,285),FixedPoint(305,275)]);
    SetPoints(pts);
    Draw(image.Bitmap, 2, [$FF662200, $FF662200, $00000000, $00000000, $00000000],0.65);

    Image.Bitmap.Canvas.TextOut(422,290,'Gradient filling of lines');

    ///////////////////////////////////////////////////

    //now just a bit of silliness to show off SimpleRadialFill ...
    pts := GetEllipsePoints(FloatRect(385,20,515,80));
    SimpleRadialFill(Image.Bitmap,pts,[$00FFFFFF,$00FFFFFF,$30FFFF00,$30FFAA00,$00FF00FF]);
    rec := Rect(385,20,515,80);
    windows.DrawText(Image.Bitmap.Canvas.handle, 'TLine32', 7,
      rec, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);

//////////////////////////////////////////////////////////////////////////////////
  finally
    free;
  end;

  //finally draw the CloseBtn ...
  closeBtnTextRec := Rect(415, 337, 505, 373);
  SimpleFill(Image.Bitmap,CloseBtnPts,$0,clBtnFace32);
  case btnState of
    bsHighlight:
      begin
        CloseBtn.DrawGradient(image.Bitmap, 3, [clBtnHighlight32,clBtnShadow32], -80);
        SimpleLine(Image.Bitmap,CloseBtnPts2,clCaptionActive32,true);
        Image.Bitmap.Canvas.Font.Color := clWhite;
        windows.DrawText(Image.Bitmap.Canvas.handle,
          '&Close',6,closeBtnTextRec, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
        Image.Bitmap.Canvas.Font.Color := clBlack;
        OffsetRect(closeBtnTextRec,1,1);
        windows.DrawText(Image.Bitmap.Canvas.handle,
          '&Close',6,closeBtnTextRec, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
      end;
    bsPressed:
      begin
        CloseBtn.DrawGradient(image.Bitmap, 3, [clBtnHighlight32,clBtnShadow32], 100);
        SimpleLine(Image.Bitmap,CloseBtnPts2,clCaptionActive32,true);
        OffsetRect(closeBtnTextRec,-1,-1);
        Image.Bitmap.Canvas.Font.Color := clWhite;
        windows.DrawText(Image.Bitmap.Canvas.handle,
          '&Close',6,closeBtnTextRec, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
        Image.Bitmap.Canvas.Font.Color := clBlack;
        OffsetRect(closeBtnTextRec,1,1);
        windows.DrawText(Image.Bitmap.Canvas.handle,
          '&Close',6,closeBtnTextRec, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
      end;
    bsDefault:
      begin
        CloseBtn.DrawGradient(image.Bitmap, 3, [clBtnHighlight32,clBtnShadow32], -80);
        Image.Bitmap.Canvas.Font.Color := clWhite;
        windows.DrawText(Image.Bitmap.Canvas.handle,
          '&Close',6,closeBtnTextRec, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
        Image.Bitmap.Canvas.Font.Color := clBlack;
        OffsetRect(closeBtnTextRec,1,1);
        windows.DrawText(Image.Bitmap.Canvas.handle,
          '&Close',6,closeBtnTextRec, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
      end;
  end;

  Image.Bitmap.Canvas.Font.Color := clWindowText;
  Image.Bitmap.EndUpdate;
  Image.Repaint;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    ScrollBar1.Position := ScrollBar1.Position - 2 else
    ScrollBar1.Position := ScrollBar1.Position + 2;
end;
//------------------------------------------------------------------------------

end.
