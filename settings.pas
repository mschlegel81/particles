UNIT settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls,viewWrapper,particlePhysics;

TYPE

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Label6: TLabel;
    currentScenarioLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    light2TrackBar: TTrackBar;
    setupComboBox: TComboBox;
    ScenarioProgressBar: TProgressBar;
    switchSetupButton: TButton;
    lockSetupCheckBox: TCheckBox;
    finerBallsCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    fpsTargetLabel: TLabel;
    Label2: TLabel;
    fpsMeasuredLabel: TLabel;
    Label3: TLabel;
    fpsTrackBar: TTrackBar;
    ballSizeTrackBar: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    switchTimeLabel: TLabel;
    speedTrackBar: TTrackBar;
    switchTimeTrackBar: TTrackBar;
    light1TrackBar: TTrackBar;
    updateFPSTimer: TTimer;
    PROCEDURE ballSizeTrackBarChange(Sender: TObject);
    PROCEDURE finerBallsCheckBoxChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE fpsTrackBarChange(Sender: TObject);
    procedure light1TrackBarChange(Sender: TObject);
    procedure light2TrackBarChange(Sender: TObject);
    PROCEDURE lockSetupCheckBoxChange(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE switchSetupButtonClick(Sender: TObject);
    PROCEDURE switchTimeTrackBarChange(Sender: TObject);
    PROCEDURE updateFPSTimerTimer(Sender: TObject);
  private

  public

  end;

VAR sharedViewState:T_viewState;
FUNCTION getSettingsForm:TSettingsForm;
FUNCTION isSettingsFormShowing:boolean;
IMPLEMENTATION
VAR SettingsForm: TSettingsForm=nil;
FUNCTION getSettingsForm:TSettingsForm;
  begin
    if SettingsForm=nil then SettingsForm:=TSettingsForm.Create(nil);
    result:=SettingsForm;
  end;

FUNCTION isSettingsFormShowing:boolean;
  begin
    result:=(SettingsForm<>nil) and (SettingsForm.Showing);
  end;

{ TSettingsForm }

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    setupComboBox.items.clear;
    for i:=0 to ATTRACTION_MODE_COUNT-1 do setupComboBox.items.add(ATTRACTION_MODE_NAME[i]);
    setupComboBox.items.add('<random>');
  end;

PROCEDURE TSettingsForm.ballSizeTrackBarChange(Sender: TObject);
  begin
    sharedViewState.ballSize:=0.001*ballSizeTrackBar.position;
  end;

PROCEDURE TSettingsForm.finerBallsCheckBoxChange(Sender: TObject);
  begin
    sharedViewState.finerBalls:=finerBallsCheckBox.checked;
  end;

PROCEDURE TSettingsForm.FormDestroy(Sender: TObject);
begin

end;

PROCEDURE TSettingsForm.FormResize(Sender: TObject);
begin

end;

PROCEDURE TSettingsForm.fpsTrackBarChange(Sender: TObject);
  begin
    sharedViewState.targetFPS:=fpsTrackBar.position;
    fpsTargetLabel.caption:=intToStr(sharedViewState.targetFPS);
  end;

procedure TSettingsForm.light1TrackBarChange(Sender: TObject);
begin

end;

procedure TSettingsForm.light2TrackBarChange(Sender: TObject);
begin

end;

PROCEDURE TSettingsForm.lockSetupCheckBoxChange(Sender: TObject);
  begin
    sharedViewState.ParticleEngine.lockCurrentSetup:=lockSetupCheckBox.checked;
  end;

PROCEDURE TSettingsForm.speedTrackBarChange(Sender: TObject);
  begin
    sharedViewState.ParticleEngine.TICKS_PER_SIMULATION_TIME_UNIT:=1000*exp(speedTrackBar.position/100*ln(0.1));
  end;

PROCEDURE TSettingsForm.switchSetupButtonClick(Sender: TObject);
  begin
    if setupComboBox.ItemIndex>=0
    then sharedViewState.ParticleEngine.nextSetup(sharedViewState.modeTicks,setupComboBox.ItemIndex)
    else sharedViewState.ParticleEngine.nextSetup(sharedViewState.modeTicks);
  end;

PROCEDURE TSettingsForm.switchTimeTrackBarChange(Sender: TObject);
  begin
    sharedViewState.ParticleEngine.MODE_SWITCH_INTERVAL_IN_TICKS:=switchTimeTrackBar.position*100;
    switchTimeLabel.Caption:=formatFloat('00.0',switchTimeTrackBar.position*0.1)+'s';
  end;

PROCEDURE TSettingsForm.updateFPSTimerTimer(Sender: TObject);
  begin
    fpsMeasuredLabel.caption:=formatFloat('00.0',sharedViewState.getFps);
    ScenarioProgressBar.position:=sharedViewState.modeTicks;
    currentScenarioLabel.caption:=ATTRACTION_MODE_NAME[sharedViewState.ParticleEngine.currentAttractionMode];
  end;

INITIALIZATION
  {$I settings.lrs}

finalization
  if SettingsForm<>nil then FreeAndNil(SettingsForm);

end.

