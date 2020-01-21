clear;
close all;
screen_size = [0,0,600,400];
%screen_size = [];
rng('default')
rng('shuffle')
buffer_trials = 1;

%Input subject info
expt_title = '';
prompt = {'Subject Number','Is this practice? (0 or 1)','Eye tracker mode (0 or 1)'};
default = {'10000','0', '0'};
the_button_pressed = inputdlg(prompt,expt_title,1,default);
subjectNumber = str2double(char(the_button_pressed(1,:)));
practice = str2double(char(the_button_pressed(2,:)));
eyetracking = str2double(char(the_button_pressed(3,:)));

%Parameters
%time intervals
fixation_duration = 0.75;
stimuli_duration = 1.5;
ITI = fixation_duration;
SOA = stimuli_duration+ITI;
%trail #
num_condition = 5; %nn,nd,dn,dd,and new pair
num_pairs_in_each_condition = 2; %e.g.: A1B1,A2B2 for nn
if practice == 1
    num_blocks = 2;
    repetition_per_block = 1;
else
    num_blocks = 10;
    repetition_per_block = 4;
    %     num_blocks = 2;
    %     repetition_per_block = 1;
end
num_items = 16; % # of items in an array
num_arrays_per_set = 20;
num_repeating_arrays = 16;
num_novel_arrays_per_set = 4;
% pair 1-16 are repeating ones, 17-20 are novel, 21 and 22 are buffers
if buffer_trials == 0
    num_novel_buffer_arrays_per_block = 0;
    pairs_to_choose_from = [1,2;3,4;5,6;7,8;9,10;11,12;13,14;15,16;17,18;19,20];
elseif buffer_trials == 1
    num_novel_buffer_arrays_per_block = 2;
    pairs_to_choose_from = [1,2;3,4;5,6;7,8;9,10;11,12;13,14;15,16;17,18;19,20;21,22];
end
num_novel_arrays_total = ((num_novel_arrays_per_set)*repetition_per_block+num_novel_buffer_arrays_per_block)*num_blocks;
num_novel_buffer_arrays_total = num_novel_buffer_arrays_per_block*num_blocks;
num_novel_nonbuffer_arrays_total = num_novel_arrays_total-num_novel_buffer_arrays_total;
num_arrays_total = num_repeating_arrays+num_novel_arrays_total;
num_trials_per_block = num_arrays_per_set*repetition_per_block+num_novel_buffer_arrays_per_block;
total_num_of_stimuli = num_blocks*num_trials_per_block;
list_of_keys_to_count_as_responses = [KbName('k'),KbName('l')];
target_coordinates_num = [4,6,2,5;1,3,2,5];
these_repeating_arrays_have_distractors = [[0,0,0,0],[0,1,0,1],[1,0,1,0],[1,1,1,1];];
%colors and shapes
num_colors = 2;
colors_to_choose_from = [255,0,0;0,255,0;]; %r,g,b
background_color = [127,127,127]; %grey
fixation_color = [255,255,255]; %white
fixation_radius = 15;
fond_size = 30;
line_width = 7;
break_time = 15;

%%%%%%%%%%%%%%%%%%%%%%%% start screen %%%%%%%%%%%%%%%%%%%%%%%%%%%%
screenNum = max(Screen('Screens'));
% HideCursor;
Screen('Preference', 'SkipSyncTests', 1);
[wPtr,rect] = Screen('OpenWindow',screenNum,background_color,screen_size);
xCenter = rect(3)/2;
yCenter = rect(4)/2;
length_of_the_imaginary_canvas = rect(4)/12*10;
length_of_the_imaginary_quadrant = length_of_the_imaginary_canvas/2;%note that the range to draw locations does not equal to half of half of the length
line_length = rect(4)/17;
minimal_dist_between_2_mid_points = line_length * 1.7;
fixation_location_and_size = [xCenter-fixation_radius, yCenter-fixation_radius, xCenter+fixation_radius, yCenter+fixation_radius];

% stim_list_to_present
% 6: 1total trial number, 2block trial number, 3stimuli type (0 = fixation, ~0 = stimuli), 4presentation onset time
% relative to block start time, 5presentation offset time relative to
% ,and 6novel array indication (0->repeating, 1->novel trial, 999->fixation)
% num_trials_per_block*num_blocks*2: total number of trails across all
% blocks * 2 (each trail has 1 fixation and 1 stimuli)
stim_list_to_present = TASCstimuli_order_generator_for_pairs_only(buffer_trials,num_blocks,num_condition,num_pairs_in_each_condition,repetition_per_block,num_trials_per_block,fixation_duration,stimuli_duration,subjectNumber,practice,pairs_to_choose_from);
% repeating_arrays_midpoints_coordinates (16 points per array, 2 x-y coordinates,16 arrays)
% all the mid points have least distance of minimal_dist_between_2_midDots
repeating_arrays_mid_points = TASCrepeating_arrays_midpoints_coordinates(rect,num_repeating_arrays,num_items,minimal_dist_between_2_mid_points,length_of_the_imaginary_quadrant);
% novel_arrays_midpoints_coordinates (16 points per array, 2 x-y coordinates,192 arrays)
% using num_novel_arrays_total
novel_arrays_mid_points = TASCnovel_arrays_midpoints_coordinates(rect,num_novel_arrays_total,num_items,minimal_dist_between_2_mid_points,length_of_the_imaginary_quadrant);
% possible_repeating_points_coordinates_generator:
% 6 possible points around each mid point of the repeating arrays
% 2 x-y coordinate for each of the 6 possible points & item shape (1-4)
% 16 items per arrays
% total 16 repeating arrays
% repeating_all_target_coordinates
% 2 x-y coordinates for each point for all 16 reapeating arrays
[repeating_all_possible_coordinates,repeating_all_target_coordinates,repeating_target_item_num,repeating_target_mid_points]...
    = TASCpossible_repeating_points_coordinates_generator(repeating_arrays_mid_points,num_repeating_arrays,num_items,line_length);
[novel_all_possible_coordinates,novel_all_target_coordinates,novel_target_item_num,novel_target_mid_points]...
    = TASCpossible_novel_points_coordinates_generator(novel_arrays_mid_points,num_novel_arrays_total,num_items,line_length);
% all_useful_repeating_coordinates_without_target:
% 4 points to draw two line, x1,y1;x2,y2;x3,y3;x4,y4;
% 2 x-y coordinate for each of the 4 points, and an additional column for
% color indicator (0 for non-distractors, 1 for distractor)
% 15 items per arrays, 16-1 with target
% total 16 repeating arrays
useful_repeating_coordinates_without_target = TASCuseful_repeating_coordinates_without_target_generator(repeating_target_item_num,repeating_all_possible_coordinates,num_items,num_repeating_arrays,line_width,these_repeating_arrays_have_distractors);
useful_novel_coordinates_without_target = TASCuseful_novel_coordinates_without_target_generator(num_blocks,stim_list_to_present,num_novel_nonbuffer_arrays_total,novel_target_item_num,novel_all_possible_coordinates,num_items,num_novel_arrays_total,line_width);

% show instructions
TASCinstructions;

coordinates_info = [];
results = [];

%color selection
color_for_both_n_and_d = colors_to_choose_from(datasample(1:size(colors_to_choose_from),num_colors,'Replace',false),:);
nondistractor_color = color_for_both_n_and_d(1,:);
distractor_color = color_for_both_n_and_d(2,:);

%%%%%%%%%%%%%%%%%%%%%%%%%% start of the task %%%%%%%%%%%%%%%%%%%%%%%%%%%%

if eyetracking == 1
    addpath(genpath('C:\toolbox\TobiiMatlabToolbox3.1'));
    tobii = tobii_connect('C:\toolbox\TobiiMatlabToolbox3.1\matlab_server\');
    [msg, DATA]= tobii_command(tobii,'init');
end

%Start of Block
target_coordinates=zeros(4,2,num_arrays_total,num_blocks);
run_start_time = GetSecs;
for block = 1:num_blocks
    disp('*********************************************');
    disp(['Start of Block ' num2str(block) ])
    disp('*********************************************');
    
    total_score = 0;
    
    %%%%%%%%%%%%%% trial iteration %%%%%%%%%%%%%%%
    block_start_time = GetSecs;
    
    if eyetracking == 1
        [msg, DATA]= tobii_command(tobii,'start','EyeXData\');
    end
    
    for trial = (1+ ((block-1) * num_trials_per_block * 2 ) : num_trials_per_block * 2 + ((block-1) * num_trials_per_block * 2 ))
        
        %here we are treating fixation preceeding each stimulus a seperate
        %trial, but not in saved results
        this_trial_info = stim_list_to_present(:,trial);
        array = this_trial_info(3,:);
        this_is_a_novel_trial = this_trial_info(6,:);
        onset_time_relative_to_block_start_time = this_trial_info(4,:);
        %         disp(onset_time_relative_to_block_start_time)
        offset_time_relative_to_block_start_time = this_trial_info(5,:);
        %         disp(offset_time_relative_to_block_start_time)
        % for this specific n-th stimulus: its absolute onset and offset times, which will be used in image flips
        image_absolute_onset_time = block_start_time + onset_time_relative_to_block_start_time;
        image_absolute_offset_time = block_start_time + offset_time_relative_to_block_start_time;
        
        if mod(trial,2)==1
            
            % draw fixation
            Screen('FillOval', wPtr, fixation_color, fixation_location_and_size);
            [~, stimOnset] = Screen('Flip',wPtr,image_absolute_onset_time);
            %             disp(image_absolute_onset_time)
            
            if trial>1
                if results((trial-1)/2,11)==-1 %if they didn't press any button at last trial
                    % record response at fixation period when no button was
                    % pressed (response == -1 not -2) at previous trial
                    
                    %calculate RT and display score
                    the_button_that_they_pressed=999;
                    score = 0;
                    response = -1;
                    touch = 0;
                    button_press_time = stimuli_duration;
                    how_long_we_give_them_to_respond = fixation_duration;
                    while (GetSecs - image_absolute_onset_time) < how_long_we_give_them_to_respond
                        if response == -1
                            [touch, button_press_time, keycode] = KbCheck(-1);
                            if touch == 1
                                key_pressed = find(keycode);
                                if ismember(key_pressed, list_of_keys_to_count_as_responses)
                                    %                         disp('enter ismember==1')
                                    
                                    % locate where the button is at the the array and
                                    the_button_that_they_pressed = find(list_of_keys_to_count_as_responses==key_pressed(1));
                                    if the_button_that_they_pressed == target_orientation
                                        response = 1;
                                        score = round((stimuli_duration+fixation_duration-(button_press_time-(image_absolute_onset_time-stimuli_duration)))*10,0);
                                    else
                                        response = -2;
                                        %                                 disp(['The subject pushed a valid but wrong button. It was ' KbName(find(keycode))]);
                                    end
                                else
                                    response = -2;
                                    %                             disp(['The subject pushed a invalid button. It was ' KbName(find(keycode))]);
                                end
                                % break the waiting-for-response loop if any button is pressed
                                break;
                            else
                                the_button_that_they_pressed = 999;
                            end
                        end
                    end
                    if touch ~= 1
                        the_button_that_they_pressed = 999;
                        %                 disp('RT: exceeds how long we gave them to respond');
                    end
                    
                    RT = button_press_time - (image_absolute_onset_time-stimuli_duration);
                    total_score = score + total_score;
                    
                    stim_presentation_time = ((block_start_time + real_onset_time_relative_to_block_start_time)-run_start_time);
                    
                    % save
                    results((trial-1)/2,[9:12]) = [score,RT,response, the_button_that_they_pressed];
                end
            end
            
            [~, stimOffset] = Screen('Flip',wPtr, image_absolute_offset_time);
            %             disp(image_absolute_offset_time)
            
        elseif mod(trial,2)==0
            
            disp(['This is: ' num2str(trial/2) 'th stimuli; array: ' num2str(array)]);
            
            % draw all coordinates that're not target
            all_x_coordinates_without_target = [];
            all_y_coordinates_without_target = [];
            for item = 1:num_items-1 %no target
                if this_is_a_novel_trial == 0
                    all_x_coordinates_without_target = [all_x_coordinates_without_target,useful_repeating_coordinates_without_target(:,1,item,array)'];
                    all_y_coordinates_without_target = [all_y_coordinates_without_target,useful_repeating_coordinates_without_target(:,2,item,array)'];
                else
                    all_x_coordinates_without_target = [all_x_coordinates_without_target,useful_novel_coordinates_without_target(:,1,item,array)'];
                    all_y_coordinates_without_target = [all_y_coordinates_without_target,useful_novel_coordinates_without_target(:,2,item,array)'];
                end
            end
            Screen('DrawLines', wPtr, [all_x_coordinates_without_target;all_y_coordinates_without_target], line_width, nondistractor_color);
            
            %there is no distractor here in the practice trial
            if practice == 0
                %find the distractor and re-draw it with distractor colorolor
                distractor_x = zeros(1,4);
                distractor_y = zeros(1,4);
                
                if  block > num_blocks/2
                    for item = 1:num_items-1
                        if this_is_a_novel_trial == 0
                            if useful_repeating_coordinates_without_target(1,3,item,array)+1 == 2
                                disp('repeating distractor')
            
                                distractor_x = useful_repeating_coordinates_without_target(:,1,item,array)';
                                distractor_y = useful_repeating_coordinates_without_target(:,2,item,array)';
                                Screen('DrawLines', wPtr, [distractor_x;distractor_y], line_width, distractor_color);
                            end
                        else
                            if useful_novel_coordinates_without_target(1,3,item,array)+1 == 2
                                disp('novel distractor')
                                
                                distractor_x = [useful_novel_coordinates_without_target(:,1,item,array)'];
                                distractor_y = [useful_novel_coordinates_without_target(:,2,item,array)'];
                                Screen('DrawLines', wPtr, [distractor_x;distractor_y], line_width, distractor_color);
                            end
                        end
                    end
                end
            end
            
            %draw target
            %the code blow has to be written here instead of outside the
            %loop because the specific coordinates to use for target
            if this_is_a_novel_trial == 0
                this_target_mid_point = repeating_target_mid_points(:,array)';
            else
                this_target_mid_point = novel_target_mid_points(:,array)';
            end
            target_orientation = randperm(2,1);
            %             disp(['target orientation: ' num2str(target_orientation)])
            this_target_coordinates_num = target_coordinates_num(target_orientation,:);
            if this_is_a_novel_trial == 0
                target_coordinates(:,:,array,block) = repeating_all_target_coordinates(this_target_coordinates_num,:,array);
            else
                target_coordinates(:,:,array,block) = novel_all_target_coordinates(this_target_coordinates_num,:,array);
            end
            %             disp(target_coordinates(:,:,array,block))
            target_x = target_coordinates(:,1,array,block)';
            target_y = target_coordinates(:,2,array,block)';
            Screen('DrawLines', wPtr, [target_x;target_y], line_width, nondistractor_color);
            
            [~, stimOnset] = Screen('Flip',wPtr,image_absolute_onset_time, 1);
            %             stimOnset-image_absolute_onset_time
            %             disp(['this is a novel trial: ' num2str(this_is_a_novel_trial)])
            %             disp(['ARRAY: ' num2str(array)])
            %             disp(['the desired display time is: ' num2str(onset_time_relative_to_block_start_time)])
            
            real_onset_time_relative_to_block_start_time = stimOnset-block_start_time;
            %             disp(['it is displaying at: ' num2str(real_onset_time_relative_to_block_start_time)])
            
            %calculate RT and display score
            the_button_that_they_pressed=999;
            score = 0;
            response = -1;
            touch = 0;
            button_press_time = stimuli_duration;
            how_long_we_give_them_to_respond = stimuli_duration;
            
            while (GetSecs - image_absolute_onset_time) < how_long_we_give_them_to_respond
                
                if response == -1
                    [touch, button_press_time, keycode] = KbCheck(-1);
                    if touch == 1
                        key_pressed = find(keycode);
                        if ismember(key_pressed, list_of_keys_to_count_as_responses)
                            %                         disp('enter ismember==1')
                            
                            % locate where the button is at the the array and
                            % the location is the correspondnig stim type
                            the_button_that_they_pressed = find(list_of_keys_to_count_as_responses==key_pressed(1));
                            
                            if the_button_that_they_pressed == target_orientation
                                response = 1;
                                score = round((stimuli_duration-(button_press_time-image_absolute_onset_time))*10,0);
                                %                                     disp(['The subject pushed a valid and right button. It was ' KbName(find(keycode))]);
                            else
                                response = -2;
                                %                                 disp(['The subject pushed a valid but wrong button. It was ' KbName(find(keycode))]);
                            end
                        else
                            response = -2;
                            %                             disp(['The subject pushed a invalid button. It was ' KbName(find(keycode))]);
                        end
                        % break the waiting-for-response loop if any button is pressed
                        break;
                    else
                        the_button_that_they_pressed = 999;
                    end
                end
            end
            
            if touch ~= 1
                the_button_that_they_pressed = 999;
                %                 disp('RT: exceeds how long we gave them to respond');
            end
            
            [~, stimOffset] = Screen('Flip',wPtr, image_absolute_offset_time);
            %             stimOffset-image_absolute_offset_time
            
            RT = button_press_time - image_absolute_onset_time;
            total_score = score + total_score;
            
            stim_presentation_time = ((block_start_time + real_onset_time_relative_to_block_start_time)-run_start_time);
            
            % save
            results = [ results; trial/2, block, array, minimal_dist_between_2_mid_points, ...
                line_width, line_length, stim_presentation_time, real_onset_time_relative_to_block_start_time, ...
                score, RT, response, the_button_that_they_pressed, nondistractor_color, distractor_color];
            if practice == 0
                coordinates_info = [coordinates_info; trial/2, block, array, target_orientation, ...
                    all_x_coordinates_without_target, all_y_coordinates_without_target, target_x, target_y, distractor_x, distractor_y, this_target_mid_point,this_is_a_novel_trial];
            end
        end
    end
    
    % stop recording eye tracking data at the end of each block
    if eyetracking == 1
        Screen('Flip',wPtr);
        [msg, DATA] = tobii_command(tobii,'stop');
        eyeX_left_eye{block}=load(['EyeXData\Left.txt']);
        eyeX_right_eye{block}=load(['EyeXData\Right.txt']);
        eyeX_time{block}=load(['EyeXData\Time.txt']);
    end
    
    if block ~= num_blocks
        
        if practice == 0
            %take a break
            disp('*********************************************');
            disp(['End of Block ' num2str(block)])
            disp('*********************************************');
            secCountdown = break_time : -1 : 0;
            for y = 1:length(secCountdown+1)
                starttime = GetSecs()+1;
                %instructions of 2 mins resting
                closing_instruction = ['This is the end of this block\n\n Your total score for this block is ' num2str(total_score) '\n\n Please take a short break \n\n A similar game will begin shortly\n\n Please do NOT move your chair during the break' '\n\n0 : ' num2str(secCountdown(y))];
                DrawFormattedText(wPtr, closing_instruction, 'center', 'center',[255,255,255]);
                Screen('TextSize', wPtr, fond_size);
                Screen('Flip',wPtr, starttime);
            end
            
            % start instructions
            TASCinstructions;
        end
        
    else
        
        %save
        if practice == 1
            this_subj_cond_var_name = ['TASC6subj' num2str(subjectNumber) '_practice_results'];
        else
            this_subj_cond_var_name = ['TASC6subj' num2str(subjectNumber) '_results'];
            this_subj_cond_var_name2= ['TASC6subj' num2str(subjectNumber) '_coordinates_info'];
            eval([this_subj_cond_var_name2 ' = coordinates_info;']);
            eval(['save ' this_subj_cond_var_name2 '.mat ' this_subj_cond_var_name2 ]);
        end
        eval([this_subj_cond_var_name ' = results;']);
        eval(['save ' this_subj_cond_var_name '.mat ' this_subj_cond_var_name ]);
        
        if eyetracking == 1
            if practice == 1
                this_subj_cond_var_name3= ['TASC6subj' num2str(subjectNumber) '_practice_eyetracking_results'];
                save(this_subj_cond_var_name3, 'subjectNumber','eyeX_left_eye', 'eyeX_right_eye','eyeX_time','fixation_location_and_size','block_start_time');
            else
                this_subj_cond_var_name4= ['TASC6subj' num2str(subjectNumber) '_eyetracking_results'];
                save(this_subj_cond_var_name4, 'subjectNumber','eyeX_left_eye', 'eyeX_right_eye','eyeX_time','fixation_location_and_size','block_start_time');
            end
        end
        
        if practice == 0
            %closing instruction & familiarity test
            TASCfamiliarity_test;
        else
            closing_instruction = ['Your total score for this practice block is ' num2str(total_score) '  \n\n Good Job!!!'];
            DrawFormattedText(wPtr, closing_instruction, 'center', 'center',[255,255,255]);
            Screen('TextSize', wPtr, fond_size);
            WaitSecs(6);
            Screen('Flip',wPtr);
        end
        
        if eyetracking == 1
            tobii_close(tobii);
        end
        
        if practice == 1
            proportion_correct = length(find(results(:,11)==1))/length(results(:,11));
            disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
            disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
            disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
            disp(['proportion correct in this practice is ' num2str(proportion_correct)])
            disp(['averaged RT in this practice is ' num2str(mean(results(:,10)))])
            disp('Please let the subject redo this practice is this proportion is smaller than 50%')
            disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
            disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
            disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
        end
    end
    
end

sca;
