Result:=
'uses'+LE+
' phxOpenGL_GLFW3,'+LE+
 {$IFDEF PHX_VAMPIRE}
' phxGraphics_Vampyre;'+LE+
 {$ELSE}
' phxGraphics_FreeImage;'+LE+
 {$ENDIF}
''+LE+
'constructor TGame.Create;'+LE+
'begin'+LE+
'  inherited;'+LE+
''+LE+
'end;'+LE+
''+LE+
'procedure TGame.Init;'+LE+
'begin'+LE+
'  // Create the device using the default provider'+LE+
'  Device:= TPHXDevice.Create;'+LE+
'  // Initialize the device'+LE+
'  Device.Initialize(''Phoenix Demo'', 800, 600);'+LE+
'  // Create the canvas'+LE+
'  Canvas:= Device.CreateCanvas;'+LE+
'  // Create the timer'+LE+
'  Timer:= TPHXTimer.Create;'+LE+
'end;'+LE+
''+LE+
'procedure TGame.Update;'+LE+
'begin'+LE+
'  Timer.Update;'+LE+
'  Device.Update;'+LE+
'end;'+LE+
''+LE+
'procedure TGame.Render;'+LE+
'begin'+LE+
'  Device.Clear;'+LE+
'  // Change the color to white'+LE+
'  Canvas.Color:= clrWhite;'+LE+
'  // Draw a rectangle'+LE+
'  Canvas.Rectangle(100, 100, 200, 200);'+LE+
'  // Change the color to red'+LE+
'  Canvas.Color:= clrRed;'+LE+
'  // Draw a rectangle'+LE+
'  Canvas.Rectangle(300, 100, 400, 200);'+LE+
'  // Flush the canvas'+LE+
'  Canvas.Flush;'+LE+
''+LE+
'  Device.Flip;'+LE+
'end;'+LE+
''+LE+
'procedure TGame.Shutdown;'+LE+
'begin'+LE+
'  Timer.Free;'+LE+
'  Canvas.Free;'+LE+
'  Device.Free;'+LE+
'end;'+LE+
''+LE+
{$IFDEF PHX_KEY_EVENT}
'procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);'+LE+
'begin'+LE+
'  inherited;'+LE+
'  // Terminate the application with esc'+LE+
'  if Key = VK_ESC then'+LE+
'  begin'+LE+
'    Terminate;'+LE+
'  end;'+LE+
'end;'+LE
{$ENDIF}
