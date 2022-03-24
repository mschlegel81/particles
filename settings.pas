UNIT settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, OpenGLContext,viewWrapper,particlePhysics,Process;

TYPE

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Label9: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RestartButton: TButton;
    flatShadingCheckBox: TCheckBox;
    Label6: TLabel;
    currentScenarioLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    light2TrackBar: TTrackBar;
    setupComboBox: TComboBox;
    ScenarioProgressBar: TProgressBar;
    switchSetupButton: TButton;
    lockSetupCheckBox: TCheckBox;
    hemisphereCheckBox: TCheckBox;
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
    BallQualityTrackBar: TTrackBar;
    updateFPSTimer: TTimer;
    PROCEDURE BallQualityTrackBarChange(Sender: TObject);
    PROCEDURE ballSizeTrackBarChange(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE hemisphereCheckBoxChange(Sender: TObject);
    PROCEDURE flatShadingCheckBoxChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE fpsTrackBarChange(Sender: TObject);
    PROCEDURE light1TrackBarChange(Sender: TObject);
    PROCEDURE light2TrackBarChange(Sender: TObject);
    PROCEDURE lockSetupCheckBoxChange(Sender: TObject);
    PROCEDURE OpenGlControl1DblClick(Sender: TObject);
    PROCEDURE RestartButtonClick(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE switchSetupButtonClick(Sender: TObject);
    PROCEDURE switchTimeTrackBarChange(Sender: TObject);
    PROCEDURE updateFPSTimerTimer(Sender: TObject);

    PROCEDURE IdleFunc(Sender: TObject; VAR done: boolean);
  private
    viewState:^T_viewState;
  public

  end;

VAR currentlyWindowed:boolean;
VAR SettingsForm: TSettingsForm;

IMPLEMENTATION
{ TSettingsForm }

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    if (paramCount>=1) and (paramStr(1)='-windowed') then begin
      currentlyWindowed:=true;
      WindowState:=wsNormal;
      BorderStyle:=bsSizeable;
    end else begin
      currentlyWindowed:=false;
      WindowState:=wsFullScreen;
      BorderStyle:=bsNone;
    end;
    new(viewState,create(OpenGLControl1));
    viewState^.loadFromFile(ChangeFileExt(paramStr(0),'.settings'));

    setupComboBox.items.clear;
    for i:=0 to ATTRACTION_MODE_COUNT-1 do setupComboBox.items.add(ATTRACTION_MODE_NAME[i]);
    setupComboBox.items.add('<random>');

    if currentlyWindowed
    then RestartButton.caption:='Restart in fullscreen mode'
    else RestartButton.caption:='Restart in windowed mode';

    fpsTrackBar.position:=viewState^.targetFPS;
    ballSizeTrackBar.position:=round(ln(viewState^.ballSize/0.01)/ln(0.2/0.01)*ballSizeTrackBar.max);
    BallQualityTrackBar.position:=viewState^.ballRefinement;
    hemisphereCheckBox.checked:=viewState^.hemispheres;
    flatShadingCheckBox.checked:=viewState^.flatShading;
    light1TrackBar.position:=round(255*viewState^.light1Brightness);
    light2TrackBar.position:=round(255*viewState^.light2Brightness);

    speedTrackBar.position:=round(ln(viewState^.ParticleEngine.TICKS_PER_SIMULATION_TIME_UNIT/1000)*100/ln(0.1));
    switchTimeTrackBar.position:=round(viewState^.ParticleEngine.MODE_SWITCH_INTERVAL_IN_TICKS/100);
    switchTimeLabel.caption:=formatFloat('00.0',switchTimeTrackBar.position*0.1)+'s';
    lockSetupCheckBox.checked:=viewState^.ParticleEngine.lockCurrentSetup;

    Application.OnIdle:=@IdleFunc;
  end;

PROCEDURE TSettingsForm.ballSizeTrackBarChange(Sender: TObject);
  begin
    //range: 0.001 - 0.2
    viewState^.ballSize:=0.01*exp(ln(0.2/0.01)*ballSizeTrackBar.position/ballSizeTrackBar.max);
  end;

PROCEDURE TSettingsForm.FormResize(Sender: TObject);
  begin
    Panel2.Left:=0;
    Panel2.top:=0;
    Panel2.width:=width;
    Panel2.height:=height;
  end;

PROCEDURE TSettingsForm.BallQualityTrackBarChange(Sender: TObject);
  begin
    viewState^.ballRefinement:=BallQualityTrackBar.position;
  end;

PROCEDURE TSettingsForm.hemisphereCheckBoxChange(Sender: TObject);
  begin
    viewState^.hemispheres:=hemisphereCheckBox.checked;
  end;

PROCEDURE TSettingsForm.flatShadingCheckBoxChange(Sender: TObject);
  begin
    viewState^.flatShading:=flatShadingCheckBox.checked;
  end;

PROCEDURE TSettingsForm.FormDestroy(Sender: TObject);
  begin
    viewState^.saveToFile(ChangeFileExt(paramStr(0),'.settings'));
  end;

PROCEDURE TSettingsForm.fpsTrackBarChange(Sender: TObject);
  begin
    viewState^.targetFPS:=fpsTrackBar.position;
    if viewState^.targetFPS>100
    then fpsTargetLabel.caption:='max'
    else fpsTargetLabel.caption:=intToStr(viewState^.targetFPS);
  end;

PROCEDURE TSettingsForm.light1TrackBarChange(Sender: TObject);
  begin
    viewState^.light1Brightness:=light1TrackBar.position/255;
  end;

PROCEDURE TSettingsForm.light2TrackBarChange(Sender: TObject);
  begin
    viewState^.light2Brightness:=light2TrackBar.position/255;
  end;

PROCEDURE TSettingsForm.lockSetupCheckBoxChange(Sender: TObject);
  begin
    viewState^.ParticleEngine.lockCurrentSetup:=lockSetupCheckBox.checked;
  end;

PROCEDURE TSettingsForm.OpenGlControl1DblClick(Sender: TObject);
  begin
    Panel1.visible:=not(Panel1.visible);
    if not(currentlyWindowed) and Panel1.visible then begin
      Panel2.Left:=1;
      Panel2.width:=width-1;
      Panel1.BringToFront;
    end;
  end;

PROCEDURE TSettingsForm.RestartButtonClick(Sender: TObject);
  VAR Process:TProcess;
  begin
    Process:=TProcess.create(self);
    Process.options:=[poDetached];
    Process.executable:=paramStr(0);
    if not(currentlyWindowed) then Process.parameters.add('-windowed');
    viewState^.saveToFile(ChangeFileExt(paramStr(0),'.settings'));
    Process.execute;
    halt;
  end;

PROCEDURE TSettingsForm.speedTrackBarChange(Sender: TObject);
  begin
    viewState^.ParticleEngine.TICKS_PER_SIMULATION_TIME_UNIT:=1000*exp(speedTrackBar.position/100*ln(0.1));
  end;

PROCEDURE TSettingsForm.switchSetupButtonClick(Sender: TObject);
  begin
    if setupComboBox.ItemIndex>=0
    then viewState^.ParticleEngine.nextSetup(viewState^.modeTicks,setupComboBox.ItemIndex)
    else viewState^.ParticleEngine.nextSetup(viewState^.modeTicks);
  end;

PROCEDURE TSettingsForm.switchTimeTrackBarChange(Sender: TObject);
  begin
    viewState^.ParticleEngine.MODE_SWITCH_INTERVAL_IN_TICKS:=switchTimeTrackBar.position*100;
    switchTimeLabel.caption:=formatFloat('00.0',switchTimeTrackBar.position*0.1)+'s';
  end;

PROCEDURE TSettingsForm.updateFPSTimerTimer(Sender: TObject);
  begin
    fpsMeasuredLabel.caption:=formatFloat('00.0',viewState^.getFps);
    ScenarioProgressBar.position:=viewState^.modeTicks;
    currentScenarioLabel.caption:=ATTRACTION_MODE_NAME[viewState^.ParticleEngine.currentAttractionMode];
  end;

PROCEDURE TSettingsForm.IdleFunc(Sender: TObject; VAR done: boolean);
  begin
    OpenGLControl1.Invalidate;
    done:=false; // tell lcl to handle messages and return immediatly
  end;

INITIALIZATION
  {$I settings.lrs}

end.

