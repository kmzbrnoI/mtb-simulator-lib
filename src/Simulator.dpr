////////////////////////////////////////////////////////////////////////////////
// Simulator.dpr
//  MTB simulator library.
//  Main Library file.
//  (c) Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
// 30.05.2015
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015 Michal Petrilak, Jan Horacek

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
}

{
   DESCRIPTION:

   This library is an alternative to MTB library, which provides connection
   to the MTB bus. This library does NOT connect to the MTB, however, MTB is
   simulated. Its main purpose is to siplify debugging -- MTB bus (and whole
   layout) is not necessarry, because it is simulated.

   In the main form, each MTB-UNI or MTB-TTL board is represented as one column
   of shapes. This column contains of 16 shapes = 16 inputs and outputs. You
   can view the state of all the outputs and change state of the inputs by
   clicking into the shape.

      Black border -> input=0
      Black inside -> output=0
      Green inside -> output=1
      Red border   -> input=1

  This file provides outer interface of the library, almost all functions
  are called to TForm_config (there is no technology class in the project).
}

library Simulator;

uses
  SysUtils,
  ExtCtrls,
  Math,
  Windows,
  Forms,
  Classes,
  fConfig in 'fConfig.pas' {FormConfig},
  About in 'About.pas' {F_About},
  Board in 'Board.pas' {F_Board};

{$R *.res}

var
   // this timer simulates delays on open, close, start, stop
   t_event:TTimer;

////////////////////////////////////////////////////////////////////////////////
// Simple timer activation function:

procedure ActivateTimer(callback:TNotifyEvent; interval:Integer);
begin
 t_event.OnTimer  := callback;
 t_event.Interval := interval;
 t_event.Enabled  := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function ShowConfigDialog(): Integer; stdcall;
begin
  FormConfig.Show;
  Result := 0;
end;

function HideConfigDialog(): Integer; stdcall;
begin
  FormConfig.Hide;
  Result := 0;
end;

function ShowInfoDialog(): Integer; stdcall;
begin
  Result := 50;
end;

////////////////////////////////////////////////////////////////////////////////
// This function should be called on unload:

function OnUnload(): Integer; stdcall;
begin
  FormConfig.SaveData(_CFG_FILE);
  FreeAndNil(F_About);
  FreeAndNil(F_Board);
  FreeAndNil(FormConfig);
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function ShowAboutDialog(): Integer; stdcall;
begin
  F_About.ShowModal;
  Result := 50;
end;

////////////////////////////////////////////////////////////////////////////////
// Open MTB, start scanning

function Open(): Integer; stdcall;
begin
  if (Assigned(FormConfig.PrgEvents.prgBeforeOpen)) then FormConfig.PrgEvents.prgBeforeOpen(FormConfig);
  FormConfig.status := TSimulatorStatus.opening;
  ActivateTimer(FormConfig.OnOpen, 1500);
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Close MTB

function Close(): Integer; stdcall;
begin
  if (Assigned(FormConfig.PrgEvents.prgBeforeClose)) then FormConfig.PrgEvents.prgBeforeOpen(FormConfig);
  FormConfig.status := TSimulatorStatus.closing;
  ActivateTimer(FormConfig.OnClose, 500);
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Start communication

function Start(): Integer; stdcall;
begin
  if (Assigned(FormConfig.PrgEvents.prgBeforeStart)) then FormConfig.PrgEvents.prgBeforeStart(FormConfig);
  FormConfig.status := TSimulatorStatus.starting;
  ActivateTimer(FormConfig.OnStart, 500);
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Stop communication

function Stop(): Integer; stdcall;
begin
  if (Assigned(FormConfig.PrgEvents.prgBeforeStop)) then FormConfig.PrgEvents.prgBeforeStop(FormConfig);
  FormConfig.status := TSimulatorStatus.stopping;
  ActivateTimer(FormConfig.OnStop, 500);
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function GetInput(Module, Port: Integer): Integer; stdcall;
begin
 if ((port < 0) or (port > 15)) then Exit(-2);
 if (not Modules[Module].exists) then Exit(-2);

  Result := vstup[Module, Port];
end;

function SetOutput(Module, Port, State: Integer): Integer; stdcall;
begin
  vystup[Module, Port] := State;
  if (Assigned(FormConfig)) then FormConfig.RepaintPins;    
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Set input:
//  -- special function just for simulator purposes --

function SetInput(board:integer; input:integer; State:integer): Integer; stdcall;
 begin
  if (State < 32) then
   begin
    Vstup[Board,Input] := state;
    if (Assigned(FormConfig)) then FormConfig.RepaintPins;    
    Result := 0;
   end else begin
    Result := -1;   
   end;
 end;//function

////////////////////////////////////////////////////////////////////////////////

function GetModuleExists(Module:integer):boolean;stdcall;
 begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[Module].exists
  else
    Result := false;
 end;

function GetModuleType(Module:Integer):string;stdcall;
 begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[Module].typ
  else
    Result := 'modul neexistuje';
 end;

function GetModuleName(Module:Integer):string; stdcall;
 begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[Module].name
  else
    Result := 'modul neexistuje';
 end;

////////////////////////////////////////////////////////////////////////////////

function SaveData:Integer; stdcall;
 begin
  FormConfig.SaveData(_CFG_FILE);
  Result := 0;
 end;//function

function GetLibVersion:string; stdcall;
 begin
  Result := _VERSION;
 end;//function

function GetDeviceVersion:string; stdcall;
 begin
  Result := 'VIRTUAL';
 end;//function

function GetDriverVersion:string; stdcall;
 begin
  Result := 'VIRTUAL';
 end;//function

function GetModuleFirmware(module:Integer):string; stdcall;
 begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[module].fw
  else
    Result := 'modul neexistuje';
 end;//function

function SetMtbSpeed(Speed:Integer):Integer; stdcall;
 begin
  Result := 1;
 end;

function SetModuleName(board:Integer;Name:string):Integer; stdcall;
 begin
  if (board <= _MAX_MTB) then Modules[board].name := Name;
  Result := 0;
 end;

function IsOpen():Boolean; stdcall;
 begin
  Result := (FormConfig.Status = TSimulatorStatus.stopped) or (FormConfig.Status = TSimulatorStatus.running);
 end;//function

function IsStart():Boolean; stdcall;
 begin
  Result := (FormConfig.Status = TSimulatorStatus.running);
 end;//function

////////////////////////////////////////////////////////////////////////////////
// ----- setting events begin -----

function SetBeforeOpen(ptr:TMyEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgBeforeOpen := ptr;
 Result := 0;
end;//function

function SetAfterOpen(ptr:TMyEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgAfterOpen := ptr;
 Result := 0;
end;//function

function SetBeforeClose(ptr:TMyEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgBeforeClose := ptr;
 Result := 0;
end;//function

function SetAfterClose(ptr:TMyEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgAfterClose := ptr;
 Result := 0;
end;//function

function SetBeforeStart(ptr:TMyEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgBeforeStart := ptr;
 Result := 0;
end;//function

function SetAfterStart(ptr:TMyEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgAfterStart := ptr;
 Result := 0;
end;//function

function SetBeforeStop(ptr:TMyEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgBeforeStop := ptr;
 Result := 0;
end;//function

function SetAfterStop(ptr:TMyEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgAfterStop := ptr;
 Result := 0;
end;//function

function SetOnError(ptr:TMyErrorEvent):Integer; stdcall;
begin
 FormConfig.PrgEvents.prgError := ptr;
 Result := 0;
end;//function

// ----- setting events end -----
////////////////////////////////////////////////////////////////////////////////
// Exported functions:

exports
  OnUnload name 'onunload',
  Start name 'start',
  Stop name 'stop',
  GetInput name 'getinput',
  SetOutput name 'setoutput',
  ShowConfigDialog name 'showconfigdialog',
  HideConfigDialog name 'hideconfigdialog',
  ShowAboutDialog name 'showaboutdialog',
  GetDriverVersion name 'getdriverversion',
  GetLibVersion name 'getlibversion',
  GetDeviceVersion name 'getdeviceversion',
  GetModuleFirmware name 'getmodulefirmware',
  GetModuleExists name 'getmoduleexists',
  GetModuleType name 'getmoduletype',
  GetModuleName name 'getmodulename',
  SetModuleName name 'setmodulename',
  SetMtbSpeed name 'setmtbspeed',
  SaveData name 'savedata',
  Open name 'open',
  Close name 'close',
  IsOpen name 'isopen',
  IsStart name 'isstart',

  //events
  SetBeforeOpen name 'setbeforeopen',
  SetAfterOpen name 'setafteropen',
  SetBeforeClose name 'setbeforeclose',
  SetAfterClose name 'setafterclose',
  SetBeforeStart name 'setbeforestart',
  SetAfterStart name 'setafterstart',
  SetBeforeStop name 'setbeforestop',
  SetAfterStop name 'setafterstop',
  SetOnError name 'setonerror',

  SetInput name 'setinput';


begin
  t_event := TTimer.Create(nil);
  t_event.Enabled := false;
  Application.CreateForm(TFormConfig, FormConfig);
  Application.CreateForm(TF_About, F_About);
  Application.CreateForm(TF_Board, F_Board);
end.

