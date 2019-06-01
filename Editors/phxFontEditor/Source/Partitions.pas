Unit Partitions;

Interface

uses Types;

Type
{..............................................................................}
    TPackingMode = (
        ePackingMode_BestFitFromNWCorner,
        ePackingMode_BestFitFromNECorner,
        ePackingMode_BestFitFromSECorner,
        ePackingMode_BestFitFromSWCorner,
        ePackingMode_HorizontalStrips
    );
    //TRect = Record
    //    Left,Right,Top,Bottom: Integer;
    //End;
{..............................................................................}

{..............................................................................}
    TPartition = Class
    Private
        FStripsRect                  : TRect;
        FRowHeight                   : Integer;
        FX,FY,FX2,FY2,FWidth,FHeight : Integer;
        FPackingMode                 : TPackingMode;
        FUsed                        : Boolean;
        FA,FB                        : TPartition;
    Public
        Constructor Create(Const ax,ay,ax2,ay2 : Integer;
                           Const APackingMode  : TPackingMode);
        Destructor  Destroy; Override;

        Function Insert(Const AWidth,AHeight : Integer;
                        Var   ARect          : TRect): Boolean;
     End;
{..............................................................................}

Implementation

{..............................................................................}

{..............................................................................}
//  APackingMode is to specify how to pack the rectangles.
//  When APackingMode is
//  ePackingMode_BestFitFromNWCorner, ePackingMode_BestFitFromNECorner,
//  ePackingMode_BestFitFromSWCorner or ePackingMode_BestFitFromSECorner,
//  the rectangles will be packed using best fit partitioning starting from that corner.
//
//  When APackingMode is ePackingMode_HorizontalStrips, the rectangles will be packed
//  in a single row till it can't fit anymore on the row.  When this happens it
//  will got to the next row moving down by the largest rectangle height in
//  the row just completed.
Constructor TPartition.Create(Const ax,ay,ax2,ay2 : Integer;
                              Const APackingMode  : TPackingMode);
Begin
    Inherited Create;
    FStripsRect.Left := ax;
    FStripsRect.Top  := ay;
    FRowHeight       := 0;
    FX               := ax;
    FY               := ay;
    FX2              := ax2;
    FY2              := aY2;
    FWidth           := FX2 - FX;
    FHeight          := FY2 - FY;
    FPackingMode     := APackingMode;
    FA               := Nil;
    FB               := Nil;
    FUsed            := False;
End;
{..............................................................................}

{..............................................................................}
Destructor TPartition.Destroy;
Begin
    FA.Free;
    FB.Free;
    Inherited Destroy;
End;
{..............................................................................}

{..............................................................................}
//  Inserts a rectangle into the package, returns a Rect if result is true
//  If the rectangle could not fit, the result is False
Function TPartition.Insert(Const AWidth,AHeight : Integer;
                           Var   ARect          : TRect) : Boolean;
Var
    R : TRect;
Begin
    Result := False;
    If FPackingMode = ePackingMode_HorizontalStrips Then
    Begin
        FStripsRect.Right  := FStripsRect.Left + AWidth;
        FStripsRect.Bottom := FStripsRect.Top  + AHeight;
        If (FStripsRect.Right <= FX2) And (FStripsRect.Bottom <= FY2) Then
        Begin
            Result := True;
            ARect  := FStripsRect;
            If AHeight > FRowHeight Then FRowHeight := AHeight;
        End;
        Inc(FStripsRect.Left,AWidth);
        FStripsRect.Right  := FStripsRect.Left + AWidth;
        If FStripsRect.Right > FX2 Then
        // hit right side of partition so reset x and go to next row
        Begin
            FStripsRect.Left  := FX;
            FStripsRect.Right := FStripsRect.Left + AWidth;
            Inc(FStripsRect.Top,FRowHeight);
            FStripsRect.Bottom := FStripsRect.Top  + AHeight;
            FRowHeight         := 0;
        End;
        Exit;
    End;
    If FUsed Then
    Begin
        If Assigned(FA) Then
            Result := FA.Insert(AWidth, AHeight,R);
        If (Not Result) And Assigned(FB) Then
            Result := FB.Insert(AWidth, AHeight,R);
        If Result Then
            ARect := R;
    End
    Else
    Begin
        If (AWidth <= FWidth) and (AHeight <= FHeight) Then
        Begin
            FUsed  := True;
            Result := True;
            Case FPackingMode of
                ePackingMode_BestFitFromNWCorner: Begin
                    ARect.Left   := FX;
                    ARect.Top    := FY;
                    ARect.Right  := FX + AWidth;
                    ARect.Bottom := FY + AHeight;

                    If (FWidth-AWidth) >= (FHeight-AHeight) Then
                    Begin
                        FA := TPartition.Create(FX,(FY+Aheight),(FX+AWidth), FY2, FPackingMode);
                        FB := TPartition.Create((FX+AWidth),FY, FX2, FY2, FPackingMode);
                    End
                    Else
                    Begin
                        FA := TPartition.Create((FX+AWidth),FY,FX2, (FY+AHeight), FPackingMode);
                        FB := TPartition.Create(FX,(FY+AHeight), FX2, FY2, FPackingMode);
                    End;
                End;
                ePackingMode_BestFitFromNECorner: Begin
                    ARect.Left   := FX2 - AWidth;
                    ARect.Top    := FY;
                    ARect.Right  := FX2;
                    ARect.Bottom := FY  + AHeight;

                    If (FWidth-AWidth) >= (FHeight-AHeight) Then
                    Begin
                        FA := TPartition.Create(FX2-AWidth,(FY+Aheight),FX2, FY2, FPackingMode);
                        FB := TPartition.Create(FX,FY, FX2-AWidth, FY2, FPackingMode);
                    End
                    Else
                    Begin
                        FA := TPartition.Create(FX,FY,FX2-AWidth, (FY+AHeight), FPackingMode);
                        FB := TPartition.Create(FX,(FY+AHeight), FX2, FY2, FPackingMode);
                    End;
                End;
                ePackingMode_BestFitFromSWCorner: Begin
                    ARect.Left   := FX;
                    ARect.Top    := FY2 - AHeight;
                    ARect.Right  := FX  + AWidth;
                    ARect.Bottom := FY2;

                    If (FWidth-AWidth) >= (FHeight-AHeight) Then
                    Begin
                        FA := TPartition.Create(FX,FY,(FX+AWidth), FY2-AHeight, FPackingMode);
                        FB := TPartition.Create((FX+AWidth),FY, FX2, FY2, FPackingMode);
                    End
                    Else
                    Begin
                        FA := TPartition.Create((FX+AWidth),FY2-AHeight,FX2, FY2, FPackingMode);
                        FB := TPartition.Create(FX,FY, FX2, FY2-AHeight, FPackingMode);
                    End;
                End;
                ePackingMode_BestFitFromSECorner: Begin
                    ARect.Left   := FX2 - AWidth;
                    ARect.Top    := FY2 - AHeight;
                    ARect.Right  := FX2;
                    ARect.Bottom := FY2;

                    If (FWidth-AWidth) >= (FHeight-AHeight) Then
                    Begin
                        FA := TPartition.Create(FX2-AWidth,FY,FX2, FY2-AHeight, FPackingMode);
                        FB := TPartition.Create(FX,FY, FX2-AWidth, FY2, FPackingMode);
                    End
                    Else
                    Begin
                        FA := TPartition.Create(FX,FY2-AHeight,FX2-AWidth, FY2, FPackingMode);
                        FB := TPartition.Create(FX,FY, FX2, FY2-AHeight, FPackingMode);
                    End;
                End;
            End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
End.
