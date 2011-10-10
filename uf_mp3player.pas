unit uf_mp3player;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
PairSplitter, ExtCtrls, ComCtrls, DirectShow, u_dsutils, ActiveX,messages,IniFilesEx;

const
	WM_APP=$8000;
	WM_MEDEV=WM_APP+100;
	
	s_cfg='Config';
		k_vol='Volume';
		k_fade='Fade';
		k_fade_time='FadeTime';
		k_mixer='Mixer';
		k_atonext='AutoNext';
		k_title='Title';
		k_log='Log';
		k_Top='Top';
		k_Left='Left';
		k_Width='Width';
		k_Height='Height';
		
	s_pls='Playlist';
	
	maxfadetime=2.5; //seconds
	
	sec2mtime=10000000;
type
	{ Tf_mp3player }

	Tf_mp3player = class(TForm)
		b_remove: TButton;
		b_add: TButton;
		b_stop: TButton;
		b_play: TButton;
		cb_fade: TCheckBox;
		cb_autonext: TCheckBox;
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
		psp_fade_left: TPairSplitterSide;
		psp_fade_right: TPairSplitterSide;
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
		procedure b_removeClick(Sender: TObject);
		procedure b_stopClick(Sender: TObject);
		procedure cb_autonextChange(Sender: TObject);
		procedure cb_fadeChange(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure lb_filesDblClick(Sender: TObject);
		procedure lb_filesSelectionChange(Sender: TObject; User: boolean);
		procedure lb_soundmixerChange(Sender: TObject);
		procedure psp_fade_leftResize(Sender: TObject);
		procedure psp_volumeResize(Sender: TObject);
		procedure psp_volume_leftResize(Sender: TObject);
		procedure TickTimer(Sender: TObject);
	private
		initialsetup:boolean;
		settings:TIniFileEx;
		fading:boolean;
		
		fade_start_vol:real;
		fade_target_vol:real;
		fade_start_pos:Int64;
		fade_timelen:Int64;
		
		fade_end_event:TNotifyEvent;
		fade_graph_end_event:TNotifyEvent;
		generic_graph_end_event:TNotifyEvent;
		
		dotick:Boolean;
		
		cfade_gui:integer;
		cfade_gui_max:integer;
		cfade_seconds:real;
		cfade_len:Int64;
		
		cvolume_gui:integer;
		cvolume_gui_max:integer;
		cvolume_gui_change:Boolean;
		gb:IGraphBuilder;
		renderer:IBaseFilter;
		LastDevice:String;
		LastDeviceGuid:String;
		stopdetect:Boolean;
		
		//TEMP INTERFACES:
		baudio:IBasicAudio;
		mseek:IMediaSeeking;
		medevs:IMediaEventEx;
		medctrl:IMediaControl;
		

		playnext:boolean;
		playnext_fn:String;
		playnext_dev:String;
		
		stream_dur:Int64;

		procedure RelInterfaces();
		procedure RegetInterfaces();
		
		procedure ChangeDeviceTo(devname: String);
		function LoadFile(filename: String) :IBaseFilter;
		procedure play(fn: String; device: String);
		procedure ShowMessage(const aMsg: string);
		function frac2vol(fr:real):integer;
		procedure WMMedEv(var Msg : TMessage); message WM_MEDEV;
		procedure settings2playlist;
		procedure playlist2settings;
		procedure real_stop;
		
		procedure FadeEv_CancelFade (Sender: TObject);
		procedure FadeEv_StopGraph (Sender: TObject);
	public
		isRunning:Boolean;
	end;

var
	f_mp3player: Tf_mp3player;

implementation
uses math;

{$R *.lfm}

type
	

{ TStringHolder }

TStringHolder=class (TObject)
	public
		s:String;
		class function Read (o:TObject) :String;
		constructor Create (str:String);
	end;
	
	
	class function TStringHolder.read(o: TObject): String; 
	begin
		if (o is TStringHolder) then result :=TStringHolder(o).s
		else result:='';
	end;

	constructor TStringHolder.Create(str: String); 
	begin
		s:=str;
	end;


procedure Tf_mp3player.FormCreate(Sender: TObject);
var
	cfgmix:String;
	x:integer;
	tit:String;
begin
	isRunning:=false;
	if (not CreateGraphBuilder(gb)) then begin 
		ShowMessage(LastError);
		exit ();
	end;
	EnumDevices(CLSID_AudioRendererCategory,lb_soundmixer.Items);
	initialsetup:=true;
	
	cvolume_gui_max:=psp_volume_left.Width+psp_volume_right.Width-2;
	cfade_gui_max:=psp_fade_left.Width+psp_fade_right.Width-2;
	
	psp_volume_leftResize(psp_volume_left);
	
	settings:=TIniFileEx.Create(paramstr(0)+'.cfg');
	settings.CacheUpdates:=true;
	
	cvolume_gui:=settings.ReadInteger(s_cfg,k_vol,cvolume_gui,0,cvolume_gui_max);
	if (cvolume_gui+1<>psp_volume.Position) then psp_volume.Position:=cvolume_gui;
	
	cfade_gui:=settings.ReadInteger(s_cfg,k_fade_time,cfade_gui,0,cfade_gui_max);
	if (cfade_gui+1<>psp_fade.Position) then psp_fade.Position:=cfade_gui;
	
	if (settings.ReadBool(s_cfg,k_fade,false)) then cb_fade.Checked:=true;
	if (settings.ReadBool(s_cfg,k_atonext,false)) then cb_autonext.Checked:=true;
	
	tit:=ExtractFileName(ParamStr(0));
	Caption:=settings.ReadString(s_cfg,k_title,copy(tit,1,Length(tit)-4));
	
	if (settings.ReadBool(s_cfg,k_log,false)) then e_log.Visible:=true;
	
	Left:=settings.ReadInteger(s_cfg,k_Left,Left);
	Top:=settings.ReadInteger(s_cfg,k_Top,Top);
	Width:=settings.ReadInteger(s_cfg,k_Width,Width);
	Height:=settings.ReadInteger(s_cfg,k_Height,Height);
	
	
	cfgmix:=settings.ReadString(s_cfg,k_mixer,'');
	if (cfgmix<>'') then begin
		x:=lb_soundmixer.Items.IndexOf(cfgmix);
		if (x>=0) then lb_soundmixer.ItemIndex:=x;
	end;
	settings2playlist;
	initialsetup:=false;
end;

procedure Tf_mp3player.FormDestroy(Sender: TObject);
begin
	settings.WriteInteger(s_cfg,k_Left,Left);
	settings.WriteInteger(s_cfg,k_Top,Top);
	settings.WriteInteger(s_cfg,k_Width,Width);
	settings.WriteInteger(s_cfg,k_Height,Height);
	settings.UpdateFileIfDirty;
end;


procedure Tf_mp3player.lb_filesDblClick(Sender: TObject);
begin
	b_play.Enabled:=true;
	b_play.Click;
end;

procedure Tf_mp3player.lb_filesSelectionChange(Sender: TObject; User: boolean);
begin
	b_play.Enabled:=true;
end;

procedure Tf_mp3player.lb_soundmixerChange(Sender: TObject);
begin
	settings.WriteString(s_cfg,k_mixer,lb_soundmixer.Text);
	settings.UpdateFileIfDirty;
end;

procedure Tf_mp3player.psp_fade_leftResize(Sender: TObject);
begin
	cfade_gui:=psp_fade.Position-1;
	cfade_seconds:=(maxfadetime*cfade_gui)/cfade_gui_max;
	cfade_len:=round(cfade_seconds*sec2mtime);
	settings.WriteInteger(s_cfg,k_fade_time,cfade_gui);
	//ShowMessage('fade secs: '+FloatToStr(cfade_seconds));
end;

procedure Tf_mp3player.play(fn:String; device:String);
var
	keepguids:String;
	Reader:IBaseFilter;
	outp:IPin;
begin
	if (isRunning and cb_fade.Checked) then begin
		b_stop.Click;
		playnext:=true;
		playnext_dev:=device;
		playnext_fn:=fn;
		exit;
	end;
	
	dotick:=false;
	RelInterfaces();
	StopGraph(gb,isRunning);
	if (device<>LastDevice) then begin
		keepguids:='';
		renderer:=nil;
	end else begin 
		keepguids:=LastDeviceGuid;
	end;
	FreeGraph(gb,keepguids);
	if (device<>LastDevice) then ChangeDeviceTo(device);
	reader:=LoadFile(fn);
	if (not Assigned(reader) or  not FindPin(reader,'Output',outp)) then begin
		if Assigned(reader) then begin
			gb.RemoveFilter(Reader);
			Reader:=nil;
		end;
		ShowMessage(LastError); 
		exit;
	end;
	if (Failed(gb.Render(outp))) then begin
		ShowMessage('This computer can''t render this file :(');
		outp:=Nil;
		gb.RemoveFilter(Reader);
		Reader:=nil;
		exit;
	end;
	
	RegetInterfaces();
	
	if (cb_fade.Checked) then begin
		fading:=true;
		cvolume_gui_change:=true;
		fade_start_pos:=1;
		fade_timelen:=round(cfade_seconds*sec2mtime);
		fade_start_vol:=0;
		fade_end_event:=nil;
		fade_target_vol:=cvolume_gui/cvolume_gui_max;;
		if (Assigned(baudio)) then baudio.put_Volume(frac2vol (0));
	end;
	StartGraph(gb,isRunning);
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
	evCode:integer;
	param1, param2:integer;
begin
	if (not Assigned(medevs)) then exit;
	while (medevs.GetEvent(evCode,param1,param2,0)=S_OK) do begin
		if (evCode = EC_COMPLETE) then begin
			StopGraph(gb,isRunning);
			real_stop;
		end;
		{
		if (evdebug) then begin
			ShowMessage('evdebug: c:'+IntToStr(evCode)+' p1:'+IntToStr(param1)+' p2:'+IntToStr(param1));
		end;
		}
		medevs.FreeEventParams(evCode, param1, param2);
	end;
end;



procedure Tf_mp3player.b_playClick(Sender: TObject);
begin
	if (lb_files.ItemIndex>=0) then begin 
		play (TStringHolder.Read(lb_files.Items.Objects[lb_files.ItemIndex]),lb_soundmixer.Text);
	end;
end;

procedure Tf_mp3player.b_addClick(Sender: TObject);
begin
	if (d_fn.Execute) then begin 
		lb_files.Items.AddObject(ExtractFileName(d_fn.FileName),TStringHolder.Create(d_fn.FileName));
	end;
	playlist2settings
end;

procedure Tf_mp3player.settings2playlist;
var
	sl:TStringList;
	x,l,p:integer;
	fn:String;
begin
	sl:=TStringList.Create;
	settings.ReadSectionValues(s_pls,sl);
	l:=sl.Count;
	if (l>0) then begin
		dec (l);
		for x:=0 to l do begin
			fn:=sl[x];
			p:=pos('=',fn);
			if (p>0) then system.Delete(fn,1,p);
			lb_files.Items.AddObject(ExtractFileName(fn),TStringHolder.Create(fn));
		end;
	end;
end;

procedure Tf_mp3player.playlist2settings ;
var
	x,l:integer;
begin
	settings.EraseSection(s_pls);
	l:=lb_files.Items.Count;
	if (l>0) then begin 
		dec (l);
		for x:=0 to l do settings.WriteString(s_pls,IntToStr(x),TStringHolder.Read(lb_files.Items.Objects[x]));
	end;
	settings.UpdateFileIfDirty;
end;

procedure Tf_mp3player.real_stop;
var
	ev:TNotifyEvent;
begin
	fading:=false;
	isRunning:=false;
	ShowMessage('stop!');
	if (Assigned(fade_graph_end_event)) then begin
		ev:=fade_graph_end_event;
		fade_graph_end_event:=nil;
		ev(nil);
	end;
	if (Assigned(generic_graph_end_event)) then begin
		ev:=generic_graph_end_event;
		generic_graph_end_event:=nil;
		ev(nil);
	end;
	if (playnext) then begin
		playnext:=false;
		play(playnext_fn,playnext_dev);
	end;
end;

procedure Tf_mp3player.FadeEv_CancelFade(Sender: TObject); 
begin
	fading:=false;
end;

procedure Tf_mp3player.FadeEv_StopGraph(Sender: TObject); 
begin
	StopGraph(gb,isRunning);
	stopdetect:=true;
end;


procedure Tf_mp3player.b_removeClick(Sender: TObject);
begin
	if (lb_files.ItemIndex>=0) then begin
		if (Assigned(lb_files.Items.Objects[lb_files.ItemIndex])) then lb_files.Items.Objects[lb_files.ItemIndex].Free;
		lb_files.Items.Delete(lb_files.ItemIndex);
		playlist2settings;
	end;
end;

procedure Tf_mp3player.b_stopClick(Sender: TObject);
begin
	if (cb_fade.Checked) then begin
		fading:=true;
		mseek.GetCurrentPosition(fade_start_pos);
		fade_timelen:=round(cfade_seconds*sec2mtime);
		fade_start_vol:=cvolume_gui/cvolume_gui_max;
		fade_target_vol:=0;
		fade_end_event:=@FadeEv_StopGraph;
		stopdetect:=true;
	end else begin
		StopGraph(gb,isRunning);
		stopdetect:=true;
	end;
end;

procedure Tf_mp3player.cb_autonextChange(Sender: TObject);
begin
	settings.WriteBool(s_cfg,k_atonext,cb_autonext.Checked);
end;

procedure Tf_mp3player.cb_fadeChange(Sender: TObject);
begin
	settings.WriteBool(s_cfg,k_fade,cb_fade.Checked);
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
	fadefr:float;
	cur:Int64;
	cvol:integer;
	ev:TNotifyEvent;
	fs:TFilterState;
begin
	inc(tickid);
	if (not dotick) then exit;
	
	if (Assigned(medctrl) and stopdetect) then begin
		if (not failed(medctrl.GetState(0,fs))) then begin
			if fs=State_Stopped then begin
				stopdetect:=false;
				real_stop;
			end;
		end;
	end;
	
	if (Assigned(baudio) ) then begin
		if (cvolume_gui_change) and (not fading) then begin
			cvolume_gui_change:=false;
			
			cvol:=frac2vol (real(cvolume_gui)/cvolume_gui_max);
			baudio.put_Volume(cvol);
			settings.WriteInteger(s_cfg,k_vol,cvolume_gui);
		end;
	end;
	if (((tickid and 3 ) = 2) or fading) and Assigned(mseek) then begin
		mseek.GetCurrentPosition(cur);
		
		if (fading) and Assigned(baudio) then begin
			
			fadefr:=(cur-fade_start_pos)/fade_timelen;
			if fadefr>=1 then begin //fade over!
				ShowMessage('Fade over!');
				if (Assigned(fade_end_event)) then begin
					ev:=fade_end_event;
					fade_end_event:=nil;
					ev (nil);
				end else begin
					fading:=false;
					cvolume_gui:=round(cvolume_gui_max*fade_target_vol);
					psp_volume.Position:=cvolume_gui+1;
				end;
			end else begin
				baudio.put_Volume(frac2vol(fade_start_vol-(fade_start_vol-fade_target_vol)*fadefr));
			end;
			
			
			
		end;
		
		if (not fading ) and (stream_dur-cur<=cfade_len) and cb_fade.Checked then begin
			b_stop.Click;
			if (cb_autonext.Checked) and (lb_files.ItemIndex>=0) then begin
				playnext_dev:=lb_soundmixer.Text;
				playnext_fn:=TStringHolder.Read(lb_files.Items.Objects[lb_files.ItemIndex]);
				playnext:=true;
			end;
		end;
		
		if ((tickid and 3 ) = 2) then begin
			mseek.GetDuration(stream_dur);
			if (stream_dur=0) then pb_prog.Position:=0
			else pb_prog.Position:=round(1000*(real(cur)/stream_dur));
		end;
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
		LastDevice:=devname;
		gb.AddFilter(renderer,'Render');
		//ShowMessage('dev guid:'+LastDeviceGuid);
	end else ShowMessage(devname+' not found in audio renders?');
end;

procedure Tf_mp3player.RegetInterfaces();
var
	st,stop:Int64;
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
			st:=0;stop:=0;
			mseek.SetPositions(st,AM_SEEKING_AbsolutePositioning,stop,AM_SEEKING_NoPositioning);
		except
			mseek:=nil;
			ShowMessage('Cant get media seeking control if ATM :(');
		end;
		try
			medevs:=gb as IMediaEventEx;
			medevs.SetNotifyWindow(Handle,WM_MEDEV,0);
		except
			medevs:=nil;
			ShowMessage('Cant get media notifing control if ATM :(');
		end;
		try
			medctrl:=gb as IMediaControl;
		except
			medctrl:=nil;
			ShowMessage('Cant get media control if ATM :(');
		end;
	end
end;

procedure Tf_mp3player.RelInterfaces(); 
begin
	baudio:=nil;
	mseek:=nil;
	medevs:=nil;
	medctrl:=nil;
end;

function Tf_mp3player.LoadFile(filename: string):IBaseFilter;
var 
	s:WideString;
	frif: IFileSourceFilter;
begin
	s:=filename+#0#0;
	if (not AddFilterToGraph(gb,CLSID_AsyncReader,'reader',result)) then begin
		ShowMessage(LastError);
		Result:=nil;
		exit();
	end;

	frif:=result as IFileSourceFilter;
	if (failed(frif.Load(@s[1],nil))) then begin
		ShowMessage('Load of '+filename+' failed!');
		gb.RemoveFilter(Result);
		Result:=nil;
	end;
	frif:=nil;
end;





end.

