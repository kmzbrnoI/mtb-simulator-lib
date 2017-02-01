////////////////////////////////////////////////////////////////////////////////
// Simulator.dpr
//  MTB simulator library.
//  Main Library file.
//  (c) Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
// 09.08.2015
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015-2017 Michal Petrilak, Jan Horacek

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
  Board in 'Board.pas' {F_Board},
  LibraryEvents in 'LibraryEvents.pas';

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
// configurations files load/save

function LoadConfig(filename:PChar):Integer; stdcall;
begin

end;

function SaveConfig(filename:PChar):Integer; stdcall;
begin

end;

////////////////////////////////////////////////////////////////////////////////
// logging

procedure SetLogLevel(loglevel:Cardinal); stdcall;
begin

end;

function GetLogLevel():Cardinal; stdcall;
begin

end;

////////////////////////////////////////////////////////////////////////////////

procedure ShowConfigDialog(); stdcall;
begin
  FormConfig.Show();
end;

procedure HideConfigDialog(); stdcall;
begin
  FormConfig.Hide();
end;

////////////////////////////////////////////////////////////////////////////////
// Open MTB, start scanning

function Open():Integer; stdcall;
begin
  if (Assigned(LibEvents.BeforeOpen.event)) then LibEvents.BeforeOpen.event(FormConfig, LibEvents.BeforeOpen.data);
  FormConfig.status := TSimulatorStatus.opening;
  ActivateTimer(FormConfig.OnOpen, 1500);
end;

function OpenDevice(device:PChar; persist:boolean):Integer; stdcall;
begin

end;

////////////////////////////////////////////////////////////////////////////////
// Close MTB

function Close():Integer; stdcall;
begin
  if (Assigned(LibEvents.BeforeClose.event)) then LibEvents.BeforeClose.event(FormConfig, LibEvents.BeforeClose.data);
  FormConfig.status := TSimulatorStatus.closing;
  ActivateTimer(FormConfig.OnClose, 500);
end;

function Opened():boolean; stdcall;
begin

end;

////////////////////////////////////////////////////////////////////////////////
// Start communication

function Start():Integer; stdcall;
begin
  if (Assigned(LibEvents.BeforeStart.event)) then LibEvents.BeforeStart.event(FormConfig, LibEvents.BeforeStart.data);
  FormConfig.status := TSimulatorStatus.starting;
  ActivateTimer(FormConfig.OnStart, 500);
end;

////////////////////////////////////////////////////////////////////////////////
// Stop communication

function Stop():Integer; stdcall;
begin
  if (Assigned(LibEvents.BeforeStop.event)) then LibEvents.BeforeStop.event(FormConfig, LibEvents.BeforeStop.data);
  FormConfig.status := TSimulatorStatus.stopping;
  ActivateTimer(FormConfig.OnStop, 500);
end;

function Started():boolean; stdcall;
begin

end;

////////////////////////////////////////////////////////////////////////////////

function GetInput(module, port: Cardinal): Integer; stdcall;
begin
 if (FormConfig.Status <> TSimulatorStatus.running) then Exit(-1);
 if ((port < 0) or (port > 15)) then Exit(-2);
 if (not Modules[Module].exists) then Exit(-2);

  Result := vstup[Module, Port];
end;

function SetOutput(module, port: Cardinal; state: Integer): Integer; stdcall;
begin
  if (vystup[Module, Port] = state) then Exit(0);
  vystup[Module, Port] := State;
  FormConfig.RepaintPins;
  if (Assigned(LibEvents.OnOutputChanged.event)) then LibEvents.OnOutputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, Module);
  Result := 0;
end;

function GetOutput(module, port: Cardinal): Integer; stdcall;
begin
 if (FormConfig.Status <> TSimulatorStatus.running) then Exit(-1);
 if ((port < 0) or (port > 15)) then Exit(-2);
 if (not Modules[Module].exists) then Exit(-2);
 Result := vystup[Module, port];
end;

////////////////////////////////////////////////////////////////////////////////
// Set input:
//  -- special function just for simulator purposes --

function SetInput(module, port: Cardinal; state: Integer): Integer; stdcall;
begin
  if (State < 32) then
   begin
    Vstup[module,port] := state;
    if (Assigned(FormConfig)) then FormConfig.RepaintPins;
    Result := 0;
   end else begin
    Result := -1;
   end;
end;//function

////////////////////////////////////////////////////////////////////////////////

function GetDeviceCount():Integer; stdcall;
begin

end;

procedure GetDeviceSerial(index:Integer; serial:PChar; serialLen:Cardinal); stdcall;
begin

end;

////////////////////////////////////////////////////////////////////////////////

function IsModule(module:Cardinal):Boolean; stdcall;
begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[Module].exists
  else
    Result := false;
end;

function IsModuleFailure(module:Cardinal):Boolean; stdcall;
begin

end;

function GetModuleCount():Cardinal; stdcall;
begin

end;

function GetModuleType(Module:Cardinal):Integer; stdcall;
begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[Module].typ
  else
    Result := 'modul neexistuje';
end;

function GetModuleName(module:Cardinal; name:PChar; nameLen:Cardinal):Integer; stdcall;
begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[Module].name
  else
    Result := 'modul neexistuje';
end;

function GetModuleFW(module:Cardinal; fw:PChar; fwLen:Cardinal):Integer; stdcall;
begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[module].fw
  else
    Result := 'modul neexistuje';
end;

////////////////////////////////////////////////////////////////////////////////

function GetDeviceVersion(version:PChar; versionLen:Cardinal):Integer; stdcall;
begin
  Result := 'virtual';
end;//function

procedure GetDriverVersion(version:PChar; versionLen:Cardinal) stdcall;
begin
  Result := 'virtual';
end;//function

////////////////////////////////////////////////////////////////////////////////
// ----- setting events begin -----

procedure BindBeforeOpen(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
  LibEvents.BeforeOpen.data  := data;
  LibEvents.BeforeOpen.event := event;
end;//function

procedure BindAfterOpen(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
  LibEvents.AfterOpen.data  := data;
  LibEvents.AfterOpen.event := event;
end;//function

procedure BindBeforeClose(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
  LibEvents.BeforeClose.data  := data;
  LibEvents.BeforeClose.event := event;
end;//function

procedure BindAfterClose(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
  LibEvents.AfterClose.data  := data;
  LibEvents.AfterClose.event := event;
end;//function

procedure BindBeforeStart(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
  LibEvents.BeforeStart.data  := data;
  LibEvents.BeforeStart.event := event;
end;//function

procedure BindAfterStart(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
  LibEvents.AfterStart.data  := data;
  LibEvents.AfterStart.event := event;
end;//function

procedure BindBeforeStop(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
  LibEvents.BeforeStop.data  := data;
  LibEvents.BeforeStop.event := event;
end;//function

procedure BindAfterStop(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
  LibEvents.AfterStop.data  := data;
  LibEvents.AfterStop.event := event;
end;//function

procedure BindOnError(event:TStdErrorEvent; data:Pointer); stdcall;
begin
  LibEvents.OnError.data  := data;
  LibEvents.OnError.event := event;
end;//function

procedure BindOnLog(event:TStdLogEvent; data:Pointer); stdcall;
begin

end;

procedure BindOnInputChanged(event:TStdModuleChangeEvent; data:Pointer); stdcall;
begin
  LibEvents.OnInputChanged.data  := data;
  LibEvents.OnInputChanged.event := event;
end;//function

procedure BindOnOutputChanged(event:TStdModuleChangeEvent; data:Pointer); stdcall;
begin
  LibEvents.OnOutputChanged.data  := data;
  LibEvents.OnOutputChanged.event := event;
end;//function

procedure BindOnScanned(event:TStdNotifyEvent; data:Pointer); stdcall;
begin

end;

// ----- setting events end -----
////////////////////////////////////////////////////////////////////////////////
// Exported functions:

exports
  LoadConfig, SaveConfig,
  SetLogLevel, GetLogLevel,
  ShowConfigDialog, HideConfigDialog,
  Open, OpenDevice, Close, Opened, Start, Stop, Started,
  GetInput, GetOutput, SetOutput,
  GetDeviceCount, GetDeviceSerial,
  IsModule, IsModuleFailure, GetModuleCount, GetModuleType, GetModuleName, GetModuleFW,
  GetDeviceVersion, GetDriverVersion,
  BindBeforeOpen, BindAfterOpen, BindBeforeClose, BindAfterClose,
  BindBeforeStart, BindAfterStart, BindBeforeStop, BindAfterStop,
  BindOnError, BindOnLog, BindOnInputChanged, BindOnOutputChanged,
  BindOnScanned, SetInput;


begin
  t_event := TTimer.Create(nil);
  t_event.Enabled := false;
  Application.CreateForm(TFormConfig, FormConfig);
  Application.CreateForm(TF_Board, F_Board);
  end.

