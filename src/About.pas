////////////////////////////////////////////////////////////////////////////////
//  About.pas
//  MTB simulator library.
//  Simple about dialog.
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

   Simple about dialog.
}

unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TF_About = class(TForm)
    L_About1: TLabel;
    L_About2: TLabel;
    L_About3: TLabel;
    ST_about4: TStaticText;
    ST_about5: TStaticText;
    ST_about6: TStaticText;
    ST_lib_Version: TStaticText;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  F_About: TF_About;

const
 _VERSION = '2.2';

implementation

{$R *.dfm}

procedure TF_About.FormShow(Sender: TObject);
begin
 ST_lib_Version.Caption := 'Verze Simulator knihovny : '+_VERSION;
end;

end.
