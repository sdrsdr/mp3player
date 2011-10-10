unit IniFilesEx; 

{$mode objfpc}{$H+}

interface

uses
  IniFiles,classes;
Type
	
{ TIniFileEx }

	TIniFileEx=class (TIniFile)
		public
		isDirty:Boolean;
		procedure markDirty;
		
		function ReadInteger (const Section, Ident:String; const  Default,min,max: Longint): Longint; virtual;overload;
		
		procedure UpdateFile; override;
		procedure UpdateFileIfDirty;
		
		//handle dirtynes :)
		procedure WriteString(const Section, Ident, Value: String);override;
		procedure WriteInteger(const Section, Ident: string; Value: Longint); override;
		procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
		procedure WriteDate(const Section, Ident: string; Value: TDateTime); override;
		procedure WriteDateTime(const Section, Ident: string; Value: TDateTime); override;
		procedure WriteFloat(const Section, Ident: string; Value: Double); override;
		procedure WriteTime(const Section, Ident: string; Value: TDateTime); override;
		procedure WriteBinaryStream(const Section, Name: string; Value: TStream); override;
		procedure EraseSection(const Section: string); override;
		procedure DeleteKey(const Section, Ident: String); override;

	end;
	
implementation
uses sysutils;
{ TIniFileEx }

procedure TIniFileEx.markDirty; 
begin
	isDirty:=true;
end;

function TIniFileEx.ReadInteger(const Section, Ident: String; const Default, min, max: Longint): Longint;
begin
	Result:=inherited ReadInteger(Section,Ident,Default);
	if (Result<min) or(Result>max) then Result:=Default;
end;

procedure TIniFileEx.UpdateFile;
begin
	inherited UpdateFile;
	isDirty:=false;
end;

procedure TIniFileEx.UpdateFileIfDirty;
begin
	if (isDirty) then UpdateFile;
end;

procedure TIniFileEx.WriteString(const Section, Ident, Value: String);
begin
	inherited WriteString(Section, Ident, Value); isDirty:=true;
end;

procedure TIniFileEx.WriteInteger(const Section, Ident: string; Value: Longint);
begin
	inherited WriteInteger(Section, Ident, Value); isDirty:=true;
end;

procedure TIniFileEx.WriteBool(const Section, Ident: string; Value: Boolean);
begin
	inherited WriteBool(Section, Ident, Value); isDirty:=true;
end;

procedure TIniFileEx.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
	inherited WriteDate(Section, Ident, Value); isDirty:=true;
end;

procedure TIniFileEx.WriteDateTime(const Section, Ident: string; Value: TDateTime);
begin
	inherited WriteDateTime(Section, Ident, Value); isDirty:=true;
end;

procedure TIniFileEx.WriteFloat(const Section, Ident: string; Value: Double);
begin
	inherited WriteFloat(Section, Ident, Value); isDirty:=true;
end;

procedure TIniFileEx.WriteTime(const Section, Ident: string; Value: TDateTime);
begin
	inherited WriteTime(Section, Ident, Value); isDirty:=true;
end;

procedure TIniFileEx.WriteBinaryStream(const Section, Name: string; Value: TStream);
begin
	inherited WriteBinaryStream(Section, Name, Value); isDirty:=true;
end;

procedure TIniFileEx.EraseSection(const Section: string);
begin
	inherited EraseSection(Section); isDirty:=true;
end;

procedure TIniFileEx.DeleteKey(const Section, Ident: String);
begin
	inherited DeleteKey(Section, Ident); isDirty:=true;
end;



end.

