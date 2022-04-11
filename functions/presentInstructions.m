function presentInstructions(centsPerPoint)

global MainWindow black

instructStr = 'Welcome to the experiment. \n\n Press the SPACE bar to continue.';
showInstructions(instructStr);

instructStr = 'In this task you will choose between two options. \n \n After making a choice, you will either receive a chocolate M&M or hear an aversive sound (screeching microphone). \n\n Press SPACE to continue.';
showInstructions(instructStr);

instImg = imread('functions\stimuli\InstructionsCombine.png');
DiagramImg = Screen('MakeTexture', MainWindow, instImg);
Screen('DrawTexture', MainWindow, DiagramImg);
Screen(MainWindow, 'Flip');
RestrictKeysForKbCheck(KbName('Space'));   % Only accept spacebar
KbWait([], 2);


instImg = imread('functions\stimuli\DiagramCombine.png');
DiagramImg = Screen('MakeTexture', MainWindow, instImg);
Screen('DrawTexture', MainWindow, DiagramImg);
Screen(MainWindow, 'Flip');
RestrictKeysForKbCheck(KbName('Space'));   % Only accept spacebar
KbWait([], 2);


instructStr = 'There is no right or wrong option to choose in the task. \n\n On each trial there is a 50% chance of receiving an M&M and a 50% chance of hearing the sound. \n\n The option you choose does not determine what outcome (M&M or sound) you get. \n\n The outcome is the same regardless of whether you choose to Find Out Now or Keep It Secret. \n\n Press SPACE to begin the task.';
showInstructions(instructStr);

DrawFormattedText(MainWindow, 'If you have any questions, then please ask the experimenter now.\n\n Otherwise, press SPACE to continue.', 'center', 500 , black);
Screen(MainWindow, 'Flip');
KbWait([], 2);

Screen(MainWindow, 'Flip');

end

function showInstructions(insStr)

global MainWindow scr_centre black
global screenBackgroundColour

instrWin = Screen('OpenOffscreenWindow', MainWindow, screenBackgroundColour);
Screen('TextFont', instrWin, 'Calibri');
Screen('TextSize', instrWin, 32);
Screen('TextStyle', instrWin, 0);

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



