//Phoenix minimal blank unit
  result:='uses'                                                +LE+
           '  phxOpenGL_GLFW3,'                                 +LE+
           '  phxGraphics_FreeImage;'                           +LE+
           LE+ /////////////////////////////////////////////////////
           'procedure TGame.Init;'                              +LE+
           ' begin'                                             +LE+
           '  Device:= TPHXDevice.Create;'                      +LE+
           '  Device.Initialize(''Phoenix Demo'', 800, 600);'   +LE+
           '  Canvas := Device.CreateCanvas; '                  +LE+
           ' end;'                                              +LE+
           LE+ /////////////////////////////////////////////////////
           'procedure TGame.Update;'                            +LE+
           ' begin'                                             +LE+
           '  Device.Update;'                                   +LE+
           ' end;'                                              +LE+
           LE+ /////////////////////////////////////////////////////
           'procedure TGame.Render;'                            +LE+
           ' begin'                                             +LE+
           '  Device.Clear;'                                    +LE+
           '  Canvas.Flush;'                                    +LE+
           '  Device.Flip;'                                     +LE+
           ' end;'                                              +LE+
           LE+ /////////////////////////////////////////////////////
           'procedure TGame.Shutdown;'                          +LE+
           ' begin'                                             +LE+
           '  Canvas.Free;'                                     +LE+
           '  Device.Free;'                                     +LE+
           ' end;'                                              +LE; 

