////////////////////////////////////////////////////////////////////////////////
//  fConfig.pas
//  MTB simulator library.
//  Main configuration form implemetation.
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

   Main configuration form. See Simulator.dpr comments to uderstand how to
   interact with it.
}

unit fConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, IniFiles, Menus, StdCtrls, Spin;

const
  _CFG_FILE = 'mtb/data/simcfg.ini';        // Config file. Change MTB ranges to make form more synoptic.
  _MAX_MTB = 255;

type
  TMyEvent  = function(Sender:TObject):Integer of object; stdcall;
  TMyErrorEvent = function (Sender: TObject; errValue: word; errAddr: byte; errStr:string):Integer of object; stdcall;

  // Simulation status:
  TSimulatorStatus = (closed = 0, opening = 1, closing = 2, stopped = 3, starting = 4, running = 5, stopping = 6);

  // One MTB module
  TModule = record
    name:string;
    typ:string;
    fw:string;
    exists:boolean;
  end;

  // Form
  TFormConfig = class(TForm)
    GB_Error: TGroupBox;
    Label1: TLabel;
    SE_Err_id: TSpinEdit;
    Label2: TLabel;
    SE_Err_board: TSpinEdit;
    B_Error: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_ErrorClick(Sender: TObject);
    procedure SE_Err_boardKeyPress(Sender: TObject; var Key: Char);
  private
    Cfgbtn:array[0.._MAX_MTB] of TButton;                        // configuration buttons
    pin: Array[0.._MAX_MTB, 0..16] of TShape;                    // pins (= ports)

    fstatus : TSimulatorStatus;                                  // simulation status

    procedure ChangeInput(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShowAddress(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CfgBtnOnClick(Sender:TObject);
    procedure CfgBtnOnMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure SetStatus(new:TSimulatorStatus);
  public

   pins_start:Integer;
   pins_end:Integer;

    procedure RePaintPins();

    function LoadData(filename:string):Integer;
    procedure SaveData(filename:string);

    procedure OnOpen(Sender:TObject);
    procedure OnClose(Sender:TObject);
    procedure OnStart(Sender:TObject);
    procedure OnStop(Sender:TObject);

    property Status:TSimulatorStatus read fstatus write SetStatus;
  end;

var
  FormConfig: TFormConfig;

var
  vstup: Array[0.._MAX_MTB, 0..15] of Byte;                  // input states
  vystup: Array[0.._MAX_MTB, 0..15] of Byte;                 // output states
  Modules:array[0.._MAX_MTB] of TModule;                     // modules config


implementation

uses Board, LibraryEvents;

{$R *.dfm}

procedure TFormConfig.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := false;
  Hide;
end;

procedure TFormConfig.FormCreate(Sender: TObject);
var
  i, j: integer;
begin
  Self.LoadData(_CFG_FILE);

  for i := Self.pins_start to Self.pins_end do begin
    Cfgbtn[i] := TButton.Create(FormConfig);
    with (Cfgbtn[i]) do
     begin
      Parent  := FormConfig;
      Top     := 5;
      Left    := (i-Self.pins_start)*15 + 5;
      Caption := '?';
      Height  := 25;
      Width   := 13;
      Tag     := i;
      OnClick := Self.CfgBtnOnClick;
      OnMouseMove := Self.CfgBtnOnMove;
     end;
    for j := 0 to 15 do begin
      pin[i, j] := TShape.Create(FormConfig);
      with pin[i, j] do begin
      Parent := FormConfig;
      Left := (i-Self.pins_start)*15 + 5;
      Top := j*15 + 35;
      Width := 13;
      Height := 13;
      Pen.Width := 2;
      Tag := 16*i + j;
      OnMouseUp := ChangeInput;
      OnMouseMove := ShowAddress;
      end;
    end;
  end;
  RePaintPins;

  Self.ClientWidth := (((Self.pins_end-Self.pins_start)+1)*(15))+8;
  Self.ClientHeight := (16*15)+40 + GB_Error.Height + 2;

  Self.status := TSimulatorStatus.closed;
end;

procedure TFormConfig.FormDestroy(Sender: TObject);
var i,j:Integer;
begin
 Self.SaveData(_CFG_FILE);

 for i := 0 to _MAX_MTB do
  begin
   if (Assigned(Self.Cfgbtn[i])) then FreeAndNil(Self.Cfgbtn[i]);

   for j := 0 to 15 do
     if (Assigned(Self.pin[i, j])) then FreeAndNil(Self.pin[i, j]);
  end;
end;//procedure

procedure TFormConfig.ChangeInput(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Module, Port: Integer;
begin
  Module := (Sender as TShape).Tag div 16;
  Port   := (Sender as TShape).Tag mod 16;

  vstup[Module, Port] := vstup[Module, Port] XOR 1;
  RePaintPins();
  if (Assigned(LibEvents.OnInputChanged.event)) then LibEvents.OnInputChanged.event(Self, LibEvents.OnInputChanged.data, Module);
end;

procedure TFormConfig.RePaintPins;
var
  i, j: integer;
  sh: TShape;
begin
  for i := Self.pins_start to Self.pins_end do begin
    for j := 0 to 15 do begin
      sh := pin[i, j];

      sh.Brush.Color := clLime * vystup[i, j];
      sh.Pen.Color := clRed * vstup[i, j];
    end;
  end;
end;

procedure TFormConfig.ShowAddress(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Module, Port: Integer;
begin
  Module := (Sender as TShape).Tag div 16;
  Port   := (Sender as TShape).Tag mod 16;

  Caption := Format('%.3d / %.3d', [Module, Port])
end;

function TFormConfig.LoadData(filename:string):Integer;
var Ini:TMemIniFile;
    i:Integer;
begin
 Self.pins_start := 0;     // default value
 Self.pins_end   := 31;

 try
   Ini := TMemIniFile.Create(filename);
 except
   Result := 1;
   Exit;
 end;

 Self.pins_start := ini.ReadInteger('MTB', 'start', 0);
 Self.pins_end   := ini.ReadInteger('MTB', 'end', 31);

 if (Self.pins_end < Self.pins_start) then
  Self.pins_end := Self.pins_start;

 for i := 0 to _MAX_MTB do
  begin
   Modules[i].name   := Ini.ReadString('MTB'+IntToStr(i),'name','Simulator'+IntToStr(i));
   Modules[i].typ    := Ini.ReadString('MTB'+IntToStr(i),'typ','simulator');
   Modules[i].fw     := Ini.ReadString('MTB'+IntToStr(i),'fw','VIRTUAL');
   Modules[i].exists := Ini.ReadBool('MTB'+IntToStr(i),'is',true);
  end;

 Ini.Free;
 Result := 0;
end;//function

procedure TFormConfig.SaveData(filename:string);
var Ini:TMemIniFile;
    i:Integer;
begin
 // Hard-written directories
 if (not DirectoryExists('mtb')) then CreateDir('mtb');
 if (not DirectoryExists('mtb/data')) then CreateDir('mtb/data');

 try
   DeleteFile(filename);
   Ini := TMemIniFile.Create(filename);
 except
   Exit;
 end;

 ini.WriteInteger('MTB', 'start', Self.pins_start);
 ini.WriteInteger('MTB', 'end', Self.pins_end);

 for i := 0 to _MAX_MTB do
  begin
   Ini.WriteString('MTB'+IntToStr(i),'name',Modules[i].name);
   Ini.WriteString('MTB'+IntToStr(i),'typ',Modules[i].typ);
   Ini.WriteString('MTB'+IntToStr(i),'fw',Modules[i].fw);
   Ini.WriteBool('MTB'+IntToStr(i),'is',Modules[i].exists);
  end;

 Ini.UpdateFile();
 Ini.Free;
end;

procedure TFormConfig.SE_Err_boardKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = #13) then
  Self.B_ErrorClick(Self);
end;//procedure

procedure TFormConfig.B_ErrorClick(Sender: TObject);
begin
 if (Assigned(LibEvents.OnError.event)) then
  LibEvents.OnError.event(Self, LibEvents.OnError.data, Self.SE_Err_id.Value, Self.SE_Err_board.Value, '');
end;

procedure TFormConfig.CfgBtnOnClick(Sender:TObject);
begin
 F_Board.OpenForm((Sender as TButton).Tag);
end;//procedure

procedure TFormConfig.CfgBtnOnMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 Caption := Format('%.3d', [(Sender as TButton).Tag]);
end;//procedure

procedure TFormConfig.SetStatus(new:TSimulatorStatus);
begin
 case (new) of
  TSimulatorStatus.closed   : Self.Caption := 'Closed';
  TSimulatorStatus.opening  : Self.Caption := 'Opening...';
  TSimulatorStatus.stopped  : Self.Caption := 'Openned, stopped';
  TSimulatorStatus.starting : Self.Caption := 'Starting...';
  TSimulatorStatus.running  : Self.Caption := 'Running';
  TSimulatorStatus.stopping : Self.Caption := 'Stopping...';
  TSimulatorStatus.closing  : Self.Caption := 'Closing...';
 end;//case

 Self.fstatus := new;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// events from simulator timer:

procedure TFormConfig.OnOpen(Sender:TObject);
begin
  (Sender as TTimer).Enabled := false;
  status := TSimulatorStatus.stopped;
  if (Assigned(LibEvents.AfterOpen.event)) then LibEvents.AfterOpen.event(FormConfig, LibEvents.AfterOpen.data);
end;//procedure

procedure TFormConfig.OnClose(Sender:TObject);
begin
  (Sender as TTimer).Enabled := false;
  status := TSimulatorStatus.closed;
  if (Assigned(LibEvents.AfterClose.event)) then LibEvents.AfterClose.event(FormConfig, LibEvents.AfterClose.data);
end;//procedure

procedure TFormConfig.OnStart(Sender:TObject);
begin
  (Sender as TTimer).Enabled := false;
  status := TSimulatorStatus.running;
  if (Assigned(LibEvents.AfterStart.event)) then LibEvents.AfterStart.event(FormConfig, LibEvents.AfterStart.data);
end;//procedure

procedure TFormConfig.OnStop(Sender:TObject);
begin
  (Sender as TTimer).Enabled := false;
  status := TSimulatorStatus.stopped;
  if (Assigned(LibEvents.AfterStop.event)) then LibEvents.AfterStop.event(FormConfig, LibEvents.AfterStop.data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
  FreeAndNil(FormConfig);

end.
