////////////////////////////////////////////////////////////////////////////////
// Simulator.dpr
//  MTB simulator library.
//  Main Library file.
//  (c) Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
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

// JCL_DEBUG_EXPERT_INSERTJDBG OFF
library Simulator;

uses
  SysUtils,
  ExtCtrls,
  Math,
  Windows,
  Forms,
  Classes,
  IniFiles,
  fConfig in 'fConfig.pas' {FormConfig},
  Board in 'Board.pas' {F_Board},
  LibraryEvents in 'LibraryEvents.pas',
  Errors in 'Errors.pas';

{$R *.res}

type
  TAddr = 0..191;   // Rozsah povolenych adres

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
 if (FormConfig.Status > TSimulatorStatus.closed) then
   Exit(MTB_FILE_DEVICE_OPENED);

 try
   FormConfig.LoadData(filename);
   Result := 0;
 except
   on E:EIniFileException do
     Result := MTB_FILE_CANNOT_ACCESS;
   on E:Exception do
     Result := MTB_GENERAL_EXCEPTION;
 end;
end;

function SaveConfig(filename:PChar):Integer; stdcall;
begin
 try
   FormConfig.SaveData(filename);
   Result := 0;
 except
   on E:EIniFileException do
     Result := MTB_FILE_CANNOT_ACCESS;
   on E:Exception do
     Result := MTB_GENERAL_EXCEPTION;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// logging

procedure SetLogLevel(loglevel:Cardinal); stdcall;
begin

end;

function GetLogLevel():Cardinal; stdcall;
begin
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure ShowConfigDialog(); stdcall;
begin
  try
    FormConfig.Show();
  finally

  end;
end;

procedure HideConfigDialog(); stdcall;
begin
  try
    FormConfig.Hide();
  finally

  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Open MTB, start scanning

function Open():Integer; stdcall;
var i:Integer;
begin
  if (FormConfig.Status > TSimulatorStatus.closed) then
    Exit(MTB_ALREADY_OPENNED);

  for i := FormConfig.pins_start to FormConfig.pins_end do
   begin
    Modules[i].failure := false;
    F_Board.RG_Failure.ItemIndex := 0;
   end;

  try
    if (Assigned(LibEvents.BeforeOpen.event)) then LibEvents.BeforeOpen.event(FormConfig, LibEvents.BeforeOpen.data);
    FormConfig.status := TSimulatorStatus.opening;
    ActivateTimer(FormConfig.OnOpen, 1500);
    Result := 0;
  except
    Result := MTB_GENERAL_EXCEPTION;
  end;
end;

function OpenDevice(device:PChar; persist:boolean):Integer; stdcall;
begin
  Result := Open();
end;

////////////////////////////////////////////////////////////////////////////////
// Close MTB

function Close():Integer; stdcall;
begin
  if (FormConfig.Status < TSimulatorStatus.stopped) then
    Exit(MTB_NOT_OPENED);

  if (FormConfig.Status = TSimulatorStatus.starting) then
    Exit(MTB_SCANNING_NOT_FINISHED);

  try
    if (Assigned(LibEvents.BeforeClose.event)) then LibEvents.BeforeClose.event(FormConfig, LibEvents.BeforeClose.data);
    FormConfig.status := TSimulatorStatus.closing;
    ActivateTimer(FormConfig.OnClose, 500);
    Result := 0;
  except
    Result := MTB_GENERAL_EXCEPTION;
  end;
end;

function Opened():boolean; stdcall;
begin
  Result := (FormConfig.Status >= TSimulatorStatus.stopped)
end;

////////////////////////////////////////////////////////////////////////////////
// Start communication

function Start():Integer; stdcall;
var i, cnt:Cardinal;
begin
  if (FormConfig.Status > TSimulatorStatus.stopped) then
    Exit(MTB_ALREADY_STARTED);

  cnt := 0;
  for i := FormConfig.pins_start to FormConfig.pins_end do
    if (Modules[i].exists) then
      Inc(cnt);

  if (cnt = 0) then
    Exit(MTB_NO_MODULES);

  if (FormConfig.Status = TSimulatorStatus.opening) then
    Exit(MTB_SCANNING_NOT_FINISHED);

  if (FormConfig.Status < TSimulatorStatus.stopped) then
    Exit(MTB_NOT_OPENED);

  try
    if (Assigned(LibEvents.BeforeStart.event)) then LibEvents.BeforeStart.event(FormConfig, LibEvents.BeforeStart.data);
    FormConfig.status := TSimulatorStatus.starting;

    if ((F_Board.Showing) and (Modules[F_Board.OpenIndex].exists)) then
      F_Board.RG_Exists.Enabled := false;
    F_Board.RG_Failure.Enabled := false;

    ActivateTimer(FormConfig.OnStart, 500);
    Result := 0;
  except
    Result := MTB_GENERAL_EXCEPTION;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Stop communication

function Stop():Integer; stdcall;
begin
  if (FormConfig.Status < TSimulatorStatus.running) then
    Exit(MTB_NOT_STARTED);

  try
    if (Assigned(LibEvents.BeforeStop.event)) then LibEvents.BeforeStop.event(FormConfig, LibEvents.BeforeStop.data);
    FormConfig.status := TSimulatorStatus.stopping;
    F_Board.RG_Failure.Enabled := false;
    ActivateTimer(FormConfig.OnStop, 500);
    Result := 0;
  except
    Result := MTB_GENERAL_EXCEPTION;
  end;
end;

function Started():boolean; stdcall;
begin
  Result := (FormConfig.Status = TSimulatorStatus.running);
end;

////////////////////////////////////////////////////////////////////////////////

function GetInput(module, port: Cardinal): Integer; stdcall;
begin
  if (FormConfig.Status = TSimulatorStatus.starting) then Exit(MTB_INPUT_NOT_YET_SCANNED);
  if (FormConfig.Status <> TSimulatorStatus.running) then Exit(MTB_NOT_STARTED);
  if (port > 15) then Exit(MTB_PORT_INVALID_NUMBER);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not Modules[Module].exists)) then Exit(MTB_MODULE_INVALID_ADDR);
  if (Modules[Module].failure) then Exit(MTB_MODULE_FAILED);  

  Result := vstup[Module, Port];
end;

function SetOutput(module, port: Cardinal; state: Integer): Integer; stdcall;
begin
  if (FormConfig.Status <> TSimulatorStatus.running) then Exit(MTB_NOT_STARTED);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not Modules[Module].exists)) then Exit(MTB_MODULE_INVALID_ADDR);
  if (Modules[Module].failure) then Exit(MTB_MODULE_FAILED);
  if (port > 15) then Exit(MTB_PORT_INVALID_NUMBER);
  if (state > 15) then Exit(MTB_INVALID_SCOM_CODE);
  if (vystup[Module, Port] = state) then Exit(0);

  vystup[Module, Port] := State;
  FormConfig.RepaintPins;
  if (Assigned(LibEvents.OnOutputChanged.event)) then LibEvents.OnOutputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, Module);
  Result := 0;
end;

function GetOutput(module, port: Cardinal): Integer; stdcall;
begin
  if (FormConfig.Status <> TSimulatorStatus.running) then Exit(MTB_NOT_STARTED);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not Modules[Module].exists)) then Exit(MTB_MODULE_INVALID_ADDR);
  if (Modules[Module].failure) then Exit(MTB_MODULE_FAILED);
  if (port > 15) then Exit(MTB_PORT_INVALID_NUMBER);

  Result := vystup[Module, port];
end;

////////////////////////////////////////////////////////////////////////////////
// Set input:
//  -- special function just for simulator purposes --

function SetInput(module, port: Cardinal; state: Integer): Integer; stdcall;
begin
  Vstup[module,port] := state;
  if (Assigned(FormConfig)) then FormConfig.RepaintPins;
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function GetDeviceCount():Integer; stdcall;
begin
 Result := 1;
end;

procedure GetDeviceSerial(index:Integer; serial:PChar; serialLen:Cardinal); stdcall;
begin
 if (index = 0) then
   StrPLCopy(serial, 'SIM DEVICE', serialLen);
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
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Modules[Module].failure
  else
    Result := false;
end;

function GetModuleCount():Cardinal; stdcall;
var i, cnt:Cardinal;
begin
 cnt := 0;

 for i := FormConfig.pins_start to FormConfig.pins_end do
   if (Modules[i].exists) then
     Inc(cnt);

 Result := cnt;
end;

function GetModuleType(Module:Cardinal):Integer; stdcall;
begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
    Result := Integer(Modules[Module].typ)
  else
    Result := MTB_MODULE_INVALID_ADDR;
end;

function GetModuleName(module:Cardinal; name:PChar; nameLen:Cardinal):Integer; stdcall;
begin
  if (module <= 255) then
  begin
    StrPLCopy(name, Modules[Module].name, nameLen);
    Result := 0;
  end else
    Result := MTB_MODULE_INVALID_ADDR;
end;

function GetModuleFW(module:Cardinal; fw:PChar; fwLen:Cardinal):Integer; stdcall;
begin
  if ((module >= FormConfig.pins_start) and (module <= FormConfig.pins_end)
    and (FormConfig.Status >= TSimulatorStatus.stopped)) then
  begin
    StrPLCopy(fw, Modules[Module].fw, fwLen);
    Result := 0;
  end else
    Result := MTB_MODULE_INVALID_ADDR;
end;

////////////////////////////////////////////////////////////////////////////////

function GetDeviceVersion(version:PChar; versionLen:Cardinal):Integer; stdcall;
begin
  if (FormConfig.Status >= TSimulatorStatus.stopped) then
   begin
    StrPLCopy(version, 'MTB-SIMULATOR-V', versionLen);
    Result := 0;
   end else
    Result := MTB_DEVICE_DISCONNECTED;
end;//function

procedure GetDriverVersion(version:PChar; versionLen:Cardinal) stdcall;
begin
  StrPLCopy(version, 'SIMULATOR-DRIVER-V', versionLen);
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
  // no logging messages here
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
  LibEvents.OnScanned.data  := data;
  LibEvents.OnScanned.event := event;
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

