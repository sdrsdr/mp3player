unit uf_mp3player;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
PairSplitter, ExtCtrls, ComCtrls, DirectShow, u_dsutils, ActiveX,messages;

const
	WM_APP=$8000;
	WM_MEDEV=WM_APP+100;
type
	{ Tf_mp3player }

	Tf_mp3player = class(TForm)
		b_remove: TButton;
		b_add: TButton;
		b_stop: TButton;
		b_play: TButton;
		b_fn: TButton;
		cb_fade: TCheckBox;
		e_fn: TEdit;
		Label2: TLabel;
		Label3: TLabel;
		lb_soundmixer: TComboBox;
		Label1: TLabel; 
		d_fn: TOpenDialog;
		e_log: TMemo;
		lb_files: TListBox;
pb_prog: TProgressBar;
		psp_volume: TPairSplitter;
		psp_fade: TPairSplitter;
		psp_volume_left: TPairSplitterSide;
		psp_volume_right: TPairSplitterSide;
		PairSplitterSide3: TPairSplitterSide;
		PairSplitterSide4: TPairSplitterSide;
		Panel1: TPanel;
		Panel2: TPanel;
		Panel3: TPanel;
		Panel4: TPanel;
		Panel5: TPanel;
		Panel6: TPanel;
		Panel7: TPanel;
		Panel8: TPanel;
		Tick: TTimer;
		procedure b_addClick(Sender: TObject);
		procedure b_playClick(Sender: TObject);
		procedure b_fnClick(Sender: TObject);
		procedure b_removeClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure psp_volumeResize(Sender: TObject);
		procedure psp_volume_leftResize(Sender: TObject);
		procedure TickTimer(Sender: TObject);
	private
		fading:boolean;
		dotick:Boolean;
		cvolume_gui:integer;
		cvolume_gui_max:integer;
		cvolume_gui_change:Boolean;
		gb:IGraphBuilder;
		reader:IBaseFilter;
		ReaderGuid:String;
		renderer:IBaseFilter;
		LastDevice:String;
		LastDeviceGuid:String;
		
		//TEMP INTERFACES:
		baudio:IBasicAudio;
		mseek:IMediaSeeking;
		medevs:IMediaEventEx;
		procedure RelInterfaces();
		procedure RegetInterfaces();
		
		procedure ChangeDeviceTo(devname: String);
		procedure LoadFile(filename: String);
		procedure ReconnectAndPlay();
		procedure play(fn: String; device: String);
		procedure ShowMessage(const aMsg: string);
		function frac2vol(fr:real):integer;
		procedure WMMedEv(var Msg : TMessage); message WM_MEDEV;
	public
		isRunning:Boolean;
	end;

var
	f_mp3player: Tf_mp3player;

implementation
uses math;

{$R *.lfm}

type
	

	TStringHolder=class (TObject)
	public
		s:String;
		constructor Create (str:String);
	end;
	
	constructor TStringHolder.Create(str: String); 
	begin
		s:=str;
	end;


procedure Tf_mp3player.play(fn:String; device:String);
var keepguids:String;
begin
	dotick:=false;
	RelInterfaces();
	StopGraph(gb,isRunning);
	if (device<>LastDevice) then begin
		keepguids:=ReaderGuid;
		renderer:=nil;
	end else begin 
		keepguids:=ReaderGuid+' '+LastDeviceGuid;
	end;
	FreeGraph(gb,keepguids);
	if (device<>LastDevice) then ChangeDeviceTo(device);
	LoadFile(fn);
	RegetInterfaces();
	ReconnectAndPlay();
	dotick:=true;
end;

procedure Tf_mp3player.ShowMessage(const aMsg: string); 
begin
	e_log.Lines.add(aMsg);
end;

function Tf_mp3player.frac2vol(fr: real): integer; 
begin
	if (fr<0.000001) then result:=-10000
	else result:=round((1000.0*(1/fr))*log10(fr));
end;

procedure Tf_mp3player.WMMedEv(var Msg: TMessage); 
var
	done:Boolean;
	evCode:integer;
	param1, param2:integer;
begin
	if (not Assigned(medevs)) then exit;
	
	while (medevs.GetEvent(evCode, param1, param2, 0) = S_OK) do begin
		if (evCode = EC_COMPLETE) then ShowMessage('Complete!');
		medevs.FreeEventParams(evCode, param1, param2);
	end;
end;

procedure Tf_mp3player.ReconnectAndPlay();
var
	outp:IPin;
begin
	if (not FindPin(reader,'Output',outp)) then begin
		ShowMessage(LastError); exit;
	end;
	gb.Render(outp);
	StartGraph(gb,isRunning);
end;


procedure Tf_mp3player.b_playClick(Sender: TObject);
begin
	play (e_fn.Text,lb_soundmixer.Text);
end;

procedure Tf_mp3player.b_addClick(Sender: TObject);
begin
	if (d_fn.Execute) then begin 
		e_fn.Text:=d_fn.FileName;
		lb_files.Items.AddObject(ExtractFileName(e_fn.Text),TStringHolder.Create(e_fn.Text));
	end;
end;

procedure Tf_mp3player.b_fnClick(Sender: TObject);
begin
	if (d_fn.Execute) then e_fn.Text:=d_fn.FileName;
end;

procedure Tf_mp3player.b_removeClick(Sender: TObject);
begin
//    if (lb_files.SelCount)
end;

procedure Tf_mp3player.FormCreate(Sender: TObject);
begin
	isRunning:=false;
	if (not CreateGraphBuilder(gb)) then begin 
		ShowMessage(LastError);
		exit ();
	end;
	EnumDevices(CLSID_AudioRendererCategory,lb_soundmixer.Items);
	if (not AddFilterToGraph(gb,CLSID_AsyncReader,'reader',reader)) then begin
		ShowMessage(LastError);
		exit();
	end;
end;

procedure Tf_mp3player.psp_volumeResize(Sender: TObject);
begin
	cvolume_gui_max:=psp_volume_left.Width+psp_volume_right.Width-2;
	psp_volume_leftResize(psp_volume_left);
end;

procedure Tf_mp3player.psp_volume_leftResize(Sender: TObject);
begin
	if (cvolume_gui_max=0) then exit;
	cvolume_gui:=psp_volume.Position-1;
	cvolume_gui_change:=true;
end;

var tickid:cardinal=0;
procedure Tf_mp3player.TickTimer(Sender: TObject);
var
	cvol,cvolset:integer;
	cvol_tr:integer;
	pr:float;
	cur:Int64;
	max:Int64;
begin
	inc(tickid);
	if (not dotick) then exit;
	if (Assigned(baudio) ) then begin
		if (cvolume_gui_change) and (not fading) then begin
			cvolume_gui_change:=false;
			
			cvol:=frac2vol (real(cvolume_gui)/cvolume_gui_max);
			baudio.put_Volume(cvol);
			baudio.get_Volume(cvolset);
		end;
	end;
	if ((tickid and 3 ) = 2) and Assigned(mseek) then begin
		mseek.GetCurrentPosition(cur);
		mseek.GetDuration(max);
		
		if (max=0) then pb_prog.Position:=0
		else pb_prog.Position:=round(100*(real(cur)/max));
	end;
end;


procedure Tf_mp3player.ChangeDeviceTo (devname:String);
var
	g:TGuid;
begin
	LastDeviceGuid:='';
	renderer:=FindDeviceByName (CLSID_AudioRendererCategory,devname);
	if (Assigned(renderer)) then begin
		renderer.GetClassId(g);
		LastDeviceGuid:=GUIDToString(g);
		gb.AddFilter(renderer,'Render');
		ShowMessage('dev guid:'+LastDeviceGuid);
	end else ShowMessage(devname+' not found in audio renders?');
end;

procedure Tf_mp3player.RegetInterfaces(); 
begin
	if (Assigned(gb)) then begin
		try
			baudio:=gb as IBasicAudio;
			baudio.put_Volume(frac2vol (real(cvolume_gui)/cvolume_gui_max));
		except
			baudio:=nil;
			ShowMessage('Cant get a volume control ATM :(');
		end;
		
		try
			mseek:=gb as IMediaSeeking;
			mseek.SetTimeFormat(TIME_FORMAT_MEDIA_TIME);
		except
			mseek:=nil;
			ShowMessage('Cant get media seeking control ATM :(');
		end;
		try
			medevs:=gb as IMediaEventEx;
			medevs.SetNotifyWindow(Handle,WM_MEDEV,0);
		except
			medevs:=nil;
			ShowMessage('Cant get media notifing control ATM :(');
		end;
	end
end;

procedure Tf_mp3player.RelInterfaces(); 
begin
	baudio:=nil;
	mseek:=nil;
	medevs:=nil;
end;

procedure Tf_mp3player.LoadFile(filename: string);
var 
	s:WideString;
	frif: IFileSourceFilter;
begin
	s:=filename+#0#0;
	frif:=reader as IFileSourceFilter;
	if (failed(frif.Load(@s[1],nil))) then begin
		ShowMessage('Load of '+e_fn.Caption+' failed!');
	end;
	frif:=nil;
end;





end.
