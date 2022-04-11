function presentInstructionsEnd(qNum)

global MainWindow 

switch qNum 
    case 1 % allocate question
        instructStr = 'Now that you''re familiar with the relationship between doors and point rewards, imagine that you now have 100 trials to \n divide between both doors from the previous task. You should allocate the trials in order to win as many points as possible. \n\n Press SPACE to continue.';
        showInstructions(instructStr);
        
        instructStr = 'We will now show you the two doors with 50 trials allocated to door. \n Use the LEFT and RIGHT arrow keys to change the values under each door. \n Once you are happy with the number of trials allocated to each door, press the RETURN/ENTER key. \n\n Press SPACE to continue.';
        showInstructions(instructStr);
    case 2 % first to mind question
        instructStr = 'In this section of the experiment you will be presented with each of the \n doors from the decision task and asked questions about the outcomes that you experienced. \n\n Press SPACE to continue.';
        showInstructions(instructStr);
    case 3 % percent question
        instructStr = 'INSTRUCTIONS. \n\n Press SPACE to continue.';
        showInstructions(instructStr);
end

% instructStr = 'Your goal is to win as many points as possible which will be translated into actual money at the end of the experiment. \n\n Press SPACE to continue.';
% showInstructions(instructStr);

% instructStr = 'Blah Blah Blah\n\n Press SPACE to continue.';
% showInstructions(instructStr);
% 
% instructStr = 'Blah Blah Blah Blah\n\n Press SPACE to continue.';
% showInstructions(instructStr);

% DrawFormattedText(MainWindow, 'If you have any questions, then please ask the experimenter now.\n\n Otherwise, press SPACE to continue.', 'center', 500 , black);
% Screen(MainWindow, 'Flip');
% KbWait([], 2);

Screen(MainWindow, 'Flip');
end

function showInstructions(insStr)

global MainWindow scr_centre black
global screenBackgroundColour

instrWin = Screen('OpenOffscreenWindow', MainWindow, screenBackgroundColour);
Screen('TextFont', instrWin, 'Calibri');
Screen('TextSize', instrWin, 32);
Screen('TextStyle', instrWin, 1);

[~, ~, instrBox] = DrawFormattedText(instrWin, insStr, 'center', 'center' , black, [], [], [], 1.5);
instrBox_width = instrBox(3) - instrBox(1);
instrBox_height = instrBox(4) - instrBox(2);
textTop = 500;
destInstrBox = [scr_centre(1) - instrBox_width / 2   textTop   scr_centre(1) + instrBox_width / 2   textTop +  instrBox_height];
Screen('DrawTexture', MainWindow, instrWin, instrBox, destInstrBox);
Screen(MainWindow, 'Flip');

RestrictKeysForKbCheck(KbName('Space'));   % Only accept spacebar
KbWait([], 2);

Screen('Close', instrWin);
end