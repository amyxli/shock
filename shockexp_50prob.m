clear all
test = 0;

if test
    Screen('Preference', 'SkipSyncTests', 2 );      % Skips the Psychtoolbox calibrations - REMOVE THIS WHEN RUNNING FOR REAL!
end
Screen('CloseAll');
%+++++++++++++
clc;

functionFoldername = fullfile(pwd, 'functions');    % Generate file path for "functions" folder in current working directory
addpath(genpath(functionFoldername));               % Then add path to this folder and all subfolders

global screenNum MainWindow scr_centre DATA datafilename
global white black nCues nDoors nProbs
global screenBackgroundColor screenRect

% screenNum = 0;

screenNum = max(Screen('Screens'));  % Which screen to display the experiment

% Get screen resolution, and find location of centre of screen
[scrWidth, scrHeight] = Screen('WindowSize',screenNum);
res = [scrWidth scrHeight];
scr_centre = res / 2;
screenRect = [0,0,scrWidth,scrHeight];

KbName('UnifyKeyNames');    % Important for some reason to standardise keyboard input across platforms / OSs.

%---------------------------------------------------------------------------------------------------------------
% % Demographic data input

if exist('ExptData', 'dir') == 0
    mkdir('ExptData');
end
inputError = 1;
while inputError == 1
    inputError = 0;
    p_number = input('Participant number  ---> ');
    datafilename = ['ExptData\shockExp_50prob_subj', num2str(p_number), '.mat'];
    if exist(datafilename, 'file') == 2
        disp(['Data for participant ', num2str(p_number),' already exist'])
        inputError = 1;
    end
end

p_age = input('Participant age ---> ');
p_sex = 'a';
while p_sex ~= 'm' && p_sex ~= 'f' && p_sex ~= 'M' && p_sex ~= 'F' && p_sex ~= 'O' && p_sex ~= 'o'
    p_sex = input('Participant gender (M/F/O) ---> ', 's');
end

% Store demographic data
DATA.subject = p_number;
DATA.age = p_age;
DATA.sex = p_sex;
DATA.start_time = datestr(now,0);

%---------------------------------------------------------------------------------------------------------------
% generate a random seed using the clock, then use it to seed the random
% number generator
rng('shuffle');
randSeed = randi(30000);
DATA.rSeed = randSeed;
rng(randSeed);

%---------------------------------------------------------------------------------------------------------------
% Open main window and establish screen parameters

HideCursor;

% Create main window and set font
MainWindow = Screen(screenNum, 'OpenWindow', [], [], 32);
Screen('Preference', 'DefaultFontName', 'Calibri');
Screen('TextFont', MainWindow, 'Calibri');
Screen('TextSize', MainWindow, 34);
Screen('TextStyle', MainWindow, 1);

DATA.frameRate = round(Screen(MainWindow, 'FrameRate'));
white = WhiteIndex(MainWindow);
black = BlackIndex(MainWindow);
screenBackgroundColor = white;

% Some colors that may be of use
yellow = [255 255 0];
red = [255 0 0];
green = [34 177 76];
blue = [0 0 255];
% fille the main screen with background color
Screen('FillRect', MainWindow, screenBackgroundColor);
Screen(MainWindow, 'Flip');

%---------------------------------------------------------------------------------------------------------------
% Set up for running trials and Experiment Information
nDoors = 2;           % number of options presented
nCues = 3;            % Number of stimuli: happy, sad, confused
nProbs = 5;
cueNames = {'Happy', 'Sad', 'Confused'};
doorNames = {'FindOutNow', 'KeepItSecret'};
probNames = {'sn50', 'sn50', 'sn50', 'sn50', 'sn50'};                        %% insert probability names

if test
    nBlocks = 1;
    trialsPerBlock = 10;
    interTrialInterval = .1;
else
    nBlocks = 1;
    trialsPerBlock = 40;
    interTrialInterval = 1.0;
end
choiceOptions = {'A' 'B'};
numTrials = nBlocks * trialsPerBlock;
pointTally = 0;                 % initial point tally value
centsPerPoint = 1;

% Import block design matrix. Some notes:
% - Stimulus (or choice) A is always presented on the left, Stimulus B is on the right. 
[~,doorA,doorB,prob] = importfile2('2021BlockStruct_50prob.csv');

stimulusScaling = .75;         % e.g., 0.5 will shrink stimuli by a half
probScaling = .3;              % scale size of probability pictures
initialPause = 1.5; 
interStimulusInterval = 0.2;
doorXpos = [scrWidth * 0.3 scrWidth * 0.7];  % Screen X positions for doors
probXpos = scrWidth * 0.5;                     % Screen position for probability
cueYpos = scrHeight * 0.4;  % Screen Y positions for cue
feedbackYpos = scrHeight * 0.6;  % Screen Y positions for outcome feedback

%------------------------------------------------------------------------
% Convert outcomes to strings for displaying on screen
% outString = cellfun(@num2str, num2cell(outcome), 'UniformOutput', false); 

%---------------------------------------------------------------------------------------------------------------
% Present some instructions
presentInst(centsPerPoint);

%---------------------------------------------------------------------------------------------------------------
% Read in Stimuli and create necessary windows for choice task
for ii = 1:nCues
    imgMatrix=imread([cueNames{ii}, '.jpg'], 'jpg');
    stimulusHeight = size(imgMatrix,1);
    stimulusWidth = size(imgMatrix,2);
    stimulusTexture(ii) = Screen('MakeTexture', MainWindow, imgMatrix); 
end

for ii = 1:nDoors
    imgMatrix=imread([doorNames{ii},'.jpg'], 'jpg');
    doorHeight = size(imgMatrix,1);
    doorWidth = size(imgMatrix,2);
    doorTexture(ii) = Screen('MakeTexture', MainWindow, imgMatrix);
end

for ii = 1:nProbs
    imgMatrix=imread([probNames{ii},'.jpg'], 'jpg');
    probHeight = size(imgMatrix,1);
    probWidth = size(imgMatrix,2);
    probTexture(ii) = Screen('MakeTexture', MainWindow, imgMatrix);
end
    

% Scale the stimuli, in case they're too big
ScaledStimulusWidth = stimulusWidth * stimulusScaling;
ScaledStimulusHeight = stimulusHeight * stimulusScaling;
ScaledProbWidth = probWidth * probScaling;
ScaledProbHeight = probHeight * probScaling;

% Rectangle container for presenting stimuli
stimRect = CenterRect([0,0,ScaledStimulusWidth,ScaledStimulusHeight], screenRect);
probRect = CenterRect([0,0,ScaledProbWidth,ScaledProbHeight], screenRect);

choiceRect = zeros(nDoors, 4);
selectRect = zeros(nDoors, 4);

% Rectangle for presenting doors
doorRect = CenterRect([0,0,doorWidth,doorHeight], screenRect);

% Make coordinate rectangle for the cues (position in the top center of the screen)
cueRect = CenterRectOnPoint(stimRect, scr_centre(1), cueYpos);

% Create offscreen windows for the proceed button
buttonRect = [0, 0, 220, 100];
proceedButtonWindow = Screen('OpenOffscreenWindow', MainWindow, screenBackgroundColor, buttonRect);
Screen('TextFont', proceedButtonWindow, 'Calibri');
Screen('TextSize', proceedButtonWindow, 48);
Screen('TextStyle', proceedButtonWindow, 1);
Screen('FrameRect', proceedButtonWindow, black, [], 4);
DrawFormattedText(proceedButtonWindow, 'Proceed', 'center', 'center' , black, [], [], [], 1.8);
proceedButtonRect = CenterRectOnPoint(buttonRect, scr_centre(1), 980);

% This creates slots in an array that will be used to save the data.
DATA.choiceTrialInfo = zeros(numTrials, 12);   

WaitSecs(initialPause);
totTrial = 0;

config_io;
% optional step: verify that the driver was successfully installed/initialized
global cogent;
if(cogent.io.status ~= 0)
  error('inp/outp installation failed');
end 

% Parallel port (378 for chocolate machine, 3FF8 for shock machine [919 more potent shock machine])
address = hex2dec('3FF8'); % this is the address of the parallel port LTP1
byte = 1; % this will send a trigger 1 (previously for choc machine byte = 5)
outp(address,0);
num_choc = 1; % number m&ms to deliver per trial

%labchart setup
labchart_address = hex2dec('3FE8'); % Parallel ports address for LabChart markers (B2044: LPT3, top, port 1)
% Digital bytes to send to LabChart (make sure these match your Masterfile)

FON_byte = 1;
KIS_byte = 2;
shock_byte = 3;
nothing_byte = 4;
proceed_byte = 5;
% etc

%---------------------------------------------------------------------------------------------------------------
% Main experimental loop
%---------------------------------------------------------------------------------------------------------------
outcomeArray = [1, 0];
prob1 = [.01, .99];
prob25 = [.25, .75];
prob50 = [.50, .50];
prob75 = [.75, .25];
prob99 = [.99, .01];

for block = 1:nBlocks
    
    trialNum = randperm(trialsPerBlock);  % randomly shuffle the trial order for each block

    for trial = 1 : trialsPerBlock

        totTrial = totTrial + 1;

        trialDoors(1) = doorA(trialNum(trial));
        trialDoors(2) = doorB(trialNum(trial));
        trialDelay = 20;
        trialProb = prob(trialNum(trial));
        
        if trialProb == 1
            trialOut = randsample(outcomeArray, 1, true, prob1);
        elseif trialProb == 25
            trialOut = randsample(outcomeArray, 1, true, prob25);
        elseif trialProb == 50
            trialOut = randsample(outcomeArray, 1, true, prob50);
        elseif trialProb == 75
            trialOut = randsample(outcomeArray, 1, true, prob75);
        elseif trialProb == 99
            trialOut = randsample(outcomeArray, 1, true, prob99);
        end
        
        % Convert outcomes to strings for displaying on screen
        if trialOut == 1 % shock
            feedbackText = 'Shock';
        else % chocolate
            feedbackText = 'Nothing';
        end
            
        % Create offscreen window for drawing choice test stimuli into
        choiceTestWindow = Screen('OpenOffscreenWindow', MainWindow, screenBackgroundColor);

        % Make coordinate rectangles for the doors (position on the sides of the screen)
        for ii = 1:nDoors
            choiceRect(ii, :) = CenterRectOnPoint(doorRect, doorXpos(ii), scr_centre(2));
        end
        
        showprobRect = CenterRectOnPoint(probRect, probXpos, scr_centre(2));

        
        % Draw choice buttons and delay 
        Screen('DrawTexture', choiceTestWindow, doorTexture(trialDoors(1)), [], choiceRect(1,:));
        Screen('DrawTexture', choiceTestWindow, doorTexture(trialDoors(2)), [], choiceRect(2,:));
        Screen('TextSize', choiceTestWindow, 54);
        
        % show corresponding probability pictures 
       if trialProb == 1
           Screen('DrawTexture', choiceTestWindow, probTexture(1), [], showprobRect);
       elseif trialProb == 25    
           Screen('DrawTexture', choiceTestWindow, probTexture(2), [], showprobRect);
       elseif trialProb == 50
           Screen('DrawTexture', choiceTestWindow, probTexture(3), [], showprobRect);
       elseif trialProb == 75
           Screen('DrawTexture', choiceTestWindow, probTexture(4), [], showprobRect);
       elseif trialProb == 99
           Screen('DrawTexture', choiceTestWindow, probTexture(5), [], showprobRect);
       end     

%         DrawFormattedText(choiceTestWindow, ['Probability: ', num2str(trialProb)], 'center', 'center' , black, [], [], [], 1.8);
        Screen('DrawTexture', MainWindow, choiceTestWindow);

        choiceStartTime = Screen(MainWindow, 'Flip'); 
        ShowCursor('Arrow');

        numOptionsChosen = 0;
        optionChosen = 0;
        proceedButtonPresented = 0;
        proceedButtonClicked = 0;

        while proceedButtonClicked == 0
            [~, x, y, ~] = GetClicks(MainWindow, 0);
            
            feedbackWindow = Screen('OpenOffscreenWindow', MainWindow, screenBackgroundColor, cueRect);
            Screen('TextFont', feedbackWindow, 'Calibri');
            Screen('TextSize', feedbackWindow, 48);
            Screen('TextStyle', feedbackWindow, 1);
            
            for ii = 1:nDoors
                if numOptionsChosen < 1
                    if IsInRect(x, y, choiceRect(ii,:))
                        numOptionsChosen = numOptionsChosen + 1;
                        optionChosen = ii;
                        responseTime1 = GetSecs - choiceStartTime; % RT for choice
                        
                        % select the cue based on trial and choice
                        if trialDoors(ii) == 1  % chose 'find out now'
                            outp(labchart_address, FON_byte)
                            if trialOut == 1    % shock trial
                                trialCueTexture = stimulusTexture(2);
                            elseif trialOut == 0 % choc trial
                                trialCueTexture = stimulusTexture(1);
                            end
                        else                    % chose 'keep it secret'
                            outp(labchart_address, KIS_byte);
                            trialCueTexture = stimulusTexture(3);
                        end
                        
                        for iii = 1:2
                            if iii == 1
                                % Draw over the doors
                                Screen('FillRect', choiceTestWindow, screenBackgroundColor, choiceRect(1,:));
                                Screen('FillRect', choiceTestWindow, screenBackgroundColor, choiceRect(2,:));
                                Screen('FillRect', choiceTestWindow, screenBackgroundColor, showprobRect);
                            
                                % Draw the cue
                                Screen('DrawTexture', choiceTestWindow, trialCueTexture, [], cueRect);
                            elseif iii == 2
                                pause(trialDelay) % show the cue by itself before recieving outcome
                                
                                
                                % Draw over the cue
                                Screen('FillRect', choiceTestWindow, screenBackgroundColor, cueRect);
                                feedbackRect = CenterRectOnPoint(cueRect, scr_centre(1), feedbackYpos);
                                Screen('DrawTexture', choiceTestWindow, feedbackWindow, [], feedbackRect);
                                
                                % Draw the outcome feedback
                                DrawFormattedText(choiceTestWindow, feedbackText, 'center', 'center' , black, [], [], [], 1.8);
                                outp(labchart_address, shock_byte);
                            
                                %delivering shock
                                if trialOut == 1
                                    outp(labchart_address, shock_byte);
                                    fprintf('%d\n',byte)
                                    outp(address, 0);
                                    pause(.5)
                                    outp(address, byte);
                                    fprintf('filler\n')
                                    
                                % Deliver chocolate (code is MIA)
                                else
                                      outp(labchart_address, nothing_byte);
%                                     allOutLines(1) = 1; % turn on
%                                     niSession1.outputSingleScan(allOutLines);
%                                     WaitSecs(0.5);
%                                     allOutLines(1) = 0; % turn off
%                                     niSession1.outputSingleScan(allOutLines);
                                end
                            end

                            Screen('DrawTexture', MainWindow, choiceTestWindow);
                           
                            if iii == 2
                                % Update points tally
                                pointTally = pointTally + trialOut;
                            end
                            
                            Screen(MainWindow, 'Flip', [], 1);
                        end
                    end
                end
            end
            
            if numOptionsChosen == 1
                if trialOut == 0||1
                    if proceedButtonPresented == 0
                    pause(20)
                    end
                end
                % Present the proceed button after choice has been made
                Screen('DrawTexture', MainWindow, proceedButtonWindow, [], proceedButtonRect);
                Screen(MainWindow, 'Flip', [], 1);
                proceedButtonPresented = 1;
            end
            
            if IsInRect(x, y, proceedButtonRect) && proceedButtonPresented     % If they've clicked the proceed button
                responseTime = GetSecs - choiceStartTime;
                proceedButtonClicked = 1;
                outp(labchart_address, proceed_byte);
            end
        end        
        Screen(MainWindow, 'Flip', []);
        Screen(MainWindow, 'Flip', []);
        Screen('Close', feedbackWindow);
        Screen('Close', choiceTestWindow);

        HideCursor;
        
    
        % extra code for determining choice of Find Out Now or Secret
        if optionChosen == 1 % chose left
            if trialDoors(1) == 1 % risky door on the left
                chooseFind = 1;
            elseif trialDoors(1) == 2
                chooseFind = 0;  % risky door on the right
            end
        elseif optionChosen == 2 % chose right
            if trialDoors(1) == 1
                chooseFind = 0;
            elseif trialDoors(1) == 2
                chooseFind = 1;
            end
        end
        
        % Store and save trial data
        DATA.choiceTrialInfo(totTrial,:) = [totTrial, block, trial, trialProb, trialDoors(1), trialDoors(2), trialOut, trialDelay, optionChosen, responseTime, chooseFind, responseTime1];
        save(datafilename, 'DATA');

        WaitSecs(interTrialInterval);
    end
    
    if block ~= nBlocks
        DrawFormattedText(MainWindow, 'That is the end of the block.\n\n Press the SPACE bar to continue on to the next block.', 'center', 500 , black);
        Screen(MainWindow, 'Flip');
        RestrictKeysForKbCheck(KbName('space'));
        KbWait([], 2);
        Screen(MainWindow, 'Flip');
        WaitSecs(initialPause);
    else
    end
end

Screen('Close', proceedButtonWindow);
Screen('Close', stimulusTexture(:));
%---------------------------------------------------------------------------------------------------------------

% End experiment
DATA.payment = centsPerPoint*pointTally;

% Save the data for the final time
save(datafilename, 'DATA');
DATA.end_time = datestr(now,0);
save(datafilename, 'DATA');

WaitSecs(.5);
Screen(MainWindow, 'FillRect', white); % clear screen
DrawFormattedText(MainWindow, 'Experiment complete! Thank you for your participation. \n\n Please let the experimenter know that you have finished.', 'center', 'center' , black);
Screen(MainWindow, 'Flip');

RestrictKeysForKbCheck(KbName('q'));   % Only accept Q key to quit
KbWait([], 2);

rmpath(genpath(functionFoldername));       % Remove functions folder from path
Screen('Preference', 'SkipSyncTests', 0);
Screen('CloseAll');

% Print payment to console for experimenter
Payment = DATA.payment/100

clear all