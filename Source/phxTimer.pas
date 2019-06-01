////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxTimer;
//< Timer class

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  {$IFDEF WIN32}
  Windows,
  {$ELSE}
  LclIntf,
  {$ENDIF}

  phxTypes;

type

{$REGION 'TPHXTimer'}

// The timer callback thats used when the timer loop is used.
TPHXTimerEvent = procedure(Sender: TObject; FrameTime: Single) of object;

// The timer class is used to measure the time of each frame
//------------------------------------------------------------------------------
TPHXTimer = class(TObject)
  private
    // If the timer is paused or running
    FPaused: Boolean;
    // The start time of the timer
    FStartTime  : Double;
    // The time of the last frame
    FLastTime  : Double;
    // The current frame time
    FFrameTime : Double;
    // Elapsed time since the start
    FElapsedTime: Double;
    // The calculated framerate
    FFrameRate : Integer;
    // The total number of frames rendered
    FFrameCount : Int64;

    FInterval   : Double;

    // The time in seconds since we last calculated the framerate
    FFrameRateTime   : Double;
    // the number of frames since we last calculated the framerate
    FFrameRateCounter: Integer;

    // Variable for measuring time
    Measure: Double;

    procedure SetInterval(const Value: Double);
  protected
    // GetCurrentTime
    {$IFDEF WIN32}
    TimerFrequency: Int64;
    TimerFrame    : Int64;
    TimerStart    : Int64;
    {$ELSE}
    {$ENDIF}
  public
    constructor Create;
    destructor  Destroy; override;

    Procedure Reset;

    // Updates the timer
    Procedure Update;
    // Pause the time
    procedure Pause;
    // Resume the timer
    procedure Resume;

    // Waits for the next frame
    procedure Wait;

    // Returns the current time in seconds
    function GetCurrentTime: Double;

    // This function measures the time elapsed between two calls to the function
    function MeasureTime: Double;

    // The interval of the timer, in seconds. If zero then max speed.
    property Interval: Double   read FInterval   write SetInterval;
    // The time elapsed in seconds since the creation of the timer start.
    property ElapsedTime: Double   read FElapsedTime;
    // The time in seconds of the last time.
    property FrameTime: Double  read FFrameTime;
    // The current framerate, is weighted over the last 500 ms.
    property FrameRate: Integer  read FFrameRate;
    // The number of frames rendered since program start.
    property FrameCount: Int64    read FFrameCount;
  end;

{$ENDREGION}

implementation

{$REGION 'TPHXTimer'}

// TPHXTimer
//==============================================================================
constructor TPHXTimer.Create;
begin
  {$IFDEF WIN32}
	QueryPerformanceFrequency(TimerFrequency);

	QueryPerformanceCounter(TimerStart);
  {$ELSE}
  {$ENDIF}

  FStartTime    := GetCurrentTime();
  FLastTime     := FStartTime;
  FFrameRateTime:= 0;
  FInterval     := 1 / 60;
  FPaused       := False;
end;

//-------------------------------------------------------------------------------
destructor TPHXTimer.Destroy;
begin
  inherited Destroy;
end;

//-------------------------------------------------------------------------------
procedure TPHXTimer.Reset;
begin
  FStartTime    := GetCurrentTime();
  FLastTime     := FStartTime;
  FFrameRateTime:= 0;
  FPaused       := False;

  Measure:= 0;
end;

//-------------------------------------------------------------------------------
procedure TPHXTimer.Update;
var CurrentTime: Double;
begin
  if FPaused then
  begin
    FLastTime := GetCurrentTime();
    FFrameTime:= 0;

    Exit;
  end;

  // Get the current time
  CurrentTime:= GetCurrentTime();

  // Calculate the frame time
  FFrameTime:=CurrentTime - FLastTime;
  // Increase the elapsed time
  FElapsedTime:= FElapsedTime + FFrameTime;

  Inc(FFramerateCounter);

  FFrameRateTime:= FFrameRateTime + FFrameTime;

  // Update the frame rate every 500 ms
  IF (FFrameRateTime > 0.5) then
  begin
    FFrameRate        := Trunc(FFramerateCounter / FFrameRateTime );
    FFramerateCounter := 0;
    FFramerateTime    := 0;
  end;

  // Set the last time to the curren time
  FLastTime:= CurrentTime;

  // Increase the frame counter
  Inc(FFrameCount);
end;

//-------------------------------------------------------------------------------
procedure TPHXTimer.Pause;
begin
  FPaused:= True;
end;

//-------------------------------------------------------------------------------
procedure TPHXTimer.Resume;
begin
  FPaused:= False;
end;


//-------------------------------------------------------------------------------
procedure TPHXTimer.Wait;
var TimeStart : Double;
var TimeNow  : Double;
begin
  TimeStart:= GetCurrentTime;
  TimeNow  := GetCurrentTime;
  while (TimeNow - TimeStart) < (Interval - FrameTime) do
  begin
    {$IFDEF USE_GLFW}
    glfwSleep(0);
    {$ELSE}
    Sleep(0);
    {$ENDIF};

    TimeNow:=  GetCurrentTime;
  end;
end;



//-------------------------------------------------------------------------------
function TPHXTimer.MeasureTime: Double;
var Time: Double;
begin
  Time   := GetCurrentTime();
  Result := Time -  Measure;
  Measure:= Time;
end;

//-------------------------------------------------------------------------------
procedure TPHXTimer.SetInterval(const Value: Double);
begin
  FInterval := Value;
end;

//-------------------------------------------------------------------------------
function TPHXTimer.GetCurrentTime: Double;
begin
  {$IFDEF WIN32}
	QueryPerformanceCounter(TimerFrame);

  Result:= (TimerFrame - TimerStart) / TimerFrequency;
  {$ELSE}
  Result := (LclIntf.GetTickCount mod High(LongInt)) / 1000;
  {$ENDIF};
end;

{$ENDREGION}


end.
