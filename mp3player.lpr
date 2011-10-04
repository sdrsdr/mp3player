program mp3player;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uf_mp3player, u_dsutils;

{$R *.res}

begin
  Application.Initialize;
    Application.CreateForm(Tf_mp3player, f_mp3player);
  Application.Run;
end.

