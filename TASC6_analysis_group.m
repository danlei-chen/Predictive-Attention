clear all
subject = [2,3,4,5,6,7,8,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43];
% subject = [13,16,20];
% subject = [2,3,4,5,6,7,8,10,11,12,15,17,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42];
age=[96,100,97,100,100,100,99,99,98,98,95,90,100,99,100,100,99,100,100,99,100,100,100,99,100,100,100,100,98,100,96,100,100,99,97,100,100,100,99,99,100];

% 1 only did one block
% 9 at chance
%14 some blocks at chance

%Parameters
%time intervals
fixation_duration = 0.75;
stimuli_duration = 1.5;
ITI = fixation_duration;
SOA = stimuli_duration+ITI;
how_long_we_give_them_to_respond = stimuli_duration;
%trail #
num_condition = 5; %nn,nd,dn,dd,and new pair
num_pairs_in_each_condition = 2; %e.g.: A1B1,A2B2 for nn
num_blocks = 10;
repetition_per_block = 4;
num_items = 16; % # of items in an array
num_arrays_per_set = 20;
num_repeating_arrays = 16;
num_novel_arrays_per_set = 4;

%read results from all subjects into results_all
results_all = [];
coordinates_info_all = [];
buffer_results = [];
subject_order_num = 0;
for s = subject
    subject_order_num=subject_order_num+1;
    
    this_subj_cond_var_name= ['TASC6subj' num2str(s) '_results'];
    eval(['load ' this_subj_cond_var_name '.mat']);
    eval(['results = ' this_subj_cond_var_name ';']);
    resultSize = size(results);
    results(:,resultSize(2)+1) = repmat([subject_order_num],[resultSize(1),1]);
    
    buffer_trials = [];
    for block = 1:num_blocks
        buffer_trials=[buffer_trials,find(results(:,2)==block,2)'];
    end
    buffer_results = [buffer_results;results(buffer_trials,:)];
    results(buffer_trials,:)=[];
    
    results_all = [results_all; results];
    %change trial num
    %     results_all(1+(find(subject == subject)-1)*total_num_of_stimuli : (find(subject == subject)-1)*total_num_of_stimuli+total_num_of_stimuli,1) = [1+(find(subject == subject)-1)*total_num_of_stimuli : (find(subject == subject)-1)*total_num_of_stimuli+total_num_of_stimuli];
    
    this_subj_cond_var_name2= ['TASC6subj' num2str(s) '_coordinates_info'];
    eval(['load ' this_subj_cond_var_name2 '.mat']);
    eval(['coordinates_info = ' this_subj_cond_var_name2 ';']);
    resultSize = size(coordinates_info);
    coordinates_info(:,resultSize(2)+1) = repmat([subject_order_num],[coordinates_info(1),1]);
    coordinates_info(buffer_trials,:)=[];
    
    coordinates_info_all = [coordinates_info_all; coordinates_info];
end

% results order:
% 1trial, 2block, 3array, 4minimal_dist_between_2_mid_points
% 5line_width, 6line_length, 7stim_presentation_time, 8real_onset_time_relative_to_block_start_time
% 9score, 10RT, 11response, 12the_button_that_they_pressed, 13nondistractor_color, 14distractor_color

% coordinates_info: 1trial/2, 2block, 3array, 4target_orientation, ...
% 5-64all_x_coordinates_without_target, 65-124all_y_coordinates_without_target, ...
% 125-128target_x, 129-132target_y, 133-136distractor_x, 137-140distractor_y, 141-142this_target_mid_point,143this_is_a_novel_trial];

trial_index = results_all(:,1);
all_trials = 1:length(trial_index);
subject_index = results_all(:,size(results_all,2));
RT = results_all(:,10);
all_array = results_all(:,3);
with_or_without_distractor_trials = coordinates_info_all(:,140);
with_or_without_distractor_trials(with_or_without_distractor_trials~=0) = 1;
first_or_second_trials = all_array;
first_or_second_trials(mod(all_array,2)==1) = 1;
first_or_second_trials(mod(all_array,2)==0) = 2;
trials_with_distractor = find(coordinates_info_all(:,140)~=0 & coordinates_info_all(:,133)~=0);
trials_without_distractor = find(coordinates_info_all(:,140)==0 & coordinates_info_all(:,133)==0);
novel_trials_without_distractor = find(coordinates_info_all(:,140)==0 & coordinates_info_all(:,133)==0 & coordinates_info_all(:,143)==1);
novel_trials_with_distractor = find(coordinates_info_all(:,140)~=0 & coordinates_info_all(:,133)~=0 & coordinates_info_all(:,143)==1);
all_block = results_all(:,2);
novel_trials = find(coordinates_info_all(:,143)==1);
repeating_trials = find(coordinates_info_all(:,143)==0);
novel_or_repeating_trials = coordinates_info_all(:,143);
response = results_all(:,11);
button_pressed = results_all(:,12);
correct_trials = find(response==1);
incorrect_trials = find(response==-1);
%include RT outside 3 std in the incorrect trials
overall_mean_RT = mean(RT(correct_trials));
overall_std_RT = std(RT(correct_trials));
lower_boundry = overall_mean_RT-3*overall_std_RT;
higher_boundry = overall_mean_RT+3*overall_std_RT;
trials_outside_boundries = [find(RT < lower_boundry); find(RT > higher_boundry)];
outsider_trials = [];
for n = 1: length(trials_outside_boundries)
    if ismember(trials_outside_boundries(n),incorrect_trials)
    else
        outsider_trials = [outsider_trials; trials_outside_boundries(n)];
    end
end
RT_with_999_incorrect=RT;
RT_with_999_incorrect(incorrect_trials) = 999;
RT_with_999_incorrect_outsider = RT_with_999_incorrect;
RT_with_999_incorrect_outsider(outsider_trials) = 999;
incorrect_outsider_trails = [incorrect_trials;outsider_trials];
RT_correct_only = RT(correct_trials);

disp(['These ' num2str(length(subject)) ' subjects have mean prop correct ' num2str(length(find(response == 1))/length(response))]);
disp(['These ' num2str(length(subject)) ' subjects have mean reaction time ' num2str(mean(RT))]);
disp(['These ' num2str(length(subject)) ' subjects have mean reaction time correct trials only ' num2str(mean(RT(response == 1)))]);

subj_accuracy = [];
for s = 1:length(subject)
    subj_response = response(subject_index==s);
    accuracy = length(find(subj_response==1))/length(subj_response);
    subj_accuracy = [subj_accuracy;accuracy];
end
subj_accuracy = [subject',subj_accuracy];
sorted_accuracy = sortrows(subj_accuracy,2);
disp('Accuracy: ' );
disp(num2str(sorted_accuracy'));
disp(['the lower 3std away threshold is ' num2str([mean(subj_accuracy(:,2))-3*std(subj_accuracy(:,2))])]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%1 RT for each block
median_RT_for_each_block = zeros(length(subject),num_blocks);
for s = 1:length(subject)
    
    for block = 1:num_blocks
        trials_for_this_block = find(all_block==block & subject_index==s);
        RT_for_this_block = RT_with_999_incorrect(trials_for_this_block);
        RT_for_this_block = RT_for_this_block(RT_for_this_block~=999);
        mean_RT_for_this_block = median(RT_for_this_block);
        
        median_RT_for_each_block(s,block)  = mean_RT_for_this_block;
    end
end
SE_median_RT_for_each_block = nanstd(median_RT_for_each_block)/sqrt(length(subject));
if length(subject)~=1
    mean_median_RT_for_each_block = nanmean(median_RT_for_each_block);
else
    mean_median_RT_for_each_block = median_RT_for_each_block;
end
figure;
plot(mean_median_RT_for_each_block,'-o','LineWidth',2);
if length(subject)~=1
    errorbar(mean_median_RT_for_each_block, SE_median_RT_for_each_block);
end
title(['mean RT for each block'],'FontSize',14);
ylabel('RT(s)')
xlabel('block')

% %2 Accuracy for each block
% errRate_for_each_block = zeros(length(subject),num_blocks);
% for s = 1:length(subject)
%     block_subject = all_block(subject_index==s);
%     response_subject = response(subject_index==s);
%     for block = 1:num_blocks
%         trials_for_this_block = find(block_subject==block);
%         response_for_this_block = response_subject(trials_for_this_block);
%         errRate_for_this_block = length(find(response_for_this_block~=1))/length(response_for_this_block);
%
%         errRate_for_each_block(s,block)  = errRate_for_this_block;
%     end
% end
% if length(subject)~=1
%     errRate_for_each_block_mean = mean(errRate_for_each_block);
% else
%     errRate_for_each_block_mean = errRate_for_each_block;
% end
% figure;
% plot(errRate_for_each_block_mean,'-o','LineWidth',2);
% title(['Error rate for each block'],'FontSize',14);
% xlabel('block')

%3 Accuracy for each block
wrong_errRate_for_each_block = zeros(length(subject),num_blocks);
timeout_errRate_for_each_block = zeros(length(subject),num_blocks);
for s = 1:length(subject)
    block_subject = all_block(subject_index==s);
    response_subject = response(subject_index==s);
    button_pressed_subject = button_pressed(subject_index==s);
    for block = 1:num_blocks
        trials_for_this_block = find(block_subject==block);
        
        button_pressed_this_block = button_pressed_subject(trials_for_this_block);
        timeout_for_this_block = length(find(button_pressed_this_block==999))/length(button_pressed_this_block);
        
        response_for_this_block = response_subject(trials_for_this_block);
        wrong_button_pressed_this_block = length(find(response_for_this_block~=1 & button_pressed_this_block~=999))/length(response_for_this_block);
        
        wrong_errRate_for_each_block(s,block) = wrong_button_pressed_this_block;
        timeout_errRate_for_each_block(s,block) = timeout_for_this_block;
    end
end
if length(subject)~=1
    wrong_errRate_for_each_block_mean = mean(wrong_errRate_for_each_block);
    timeout_errRate_for_each_block_mean = mean(timeout_errRate_for_each_block);
else
    wrong_errRate_for_each_block_mean = wrong_errRate_for_each_block;
    timeout_errRate_for_each_block_mean = timeout_errRate_for_each_block;
end
figure;
plot([wrong_errRate_for_each_block_mean',timeout_errRate_for_each_block_mean'],'-o','LineWidth',2);
legend('wrong','time-out')
title(['Error rate for each block'],'FontSize',14);
xlabel('block')

%4
median_RT_1st = zeros(length(subject),num_blocks);
median_RT_2nd = zeros(length(subject),num_blocks);
SE_withwithout_distractors = zeros(num_blocks,2);

subject_index_1st = subject_index(intersect(find(first_or_second_trials==1),find(novel_or_repeating_trials==0)));
RT_1st = RT_with_999_incorrect(intersect(find(first_or_second_trials==1),find(novel_or_repeating_trials==0)));
block_1st = all_block(intersect(find(first_or_second_trials==1),find(novel_or_repeating_trials==0)));
subject_index_2nd = subject_index(intersect(find(first_or_second_trials==2),find(novel_or_repeating_trials==0)));
RT_2nd = RT_with_999_incorrect(intersect(find(first_or_second_trials==2),find(novel_or_repeating_trials==0)));
block_2nd = all_block(intersect(find(first_or_second_trials==2),find(novel_or_repeating_trials==0)));

for s = 1:length(subject)
    RT_with_distractor_subject = RT_1st(subject_index_1st==s);
    block_with_distractor_subject = block_1st(subject_index_1st==s);
    RT_without_distractor_subject = RT_2nd(subject_index_2nd==s);
    block_without_distractor_subject = block_2nd(subject_index_2nd==s);
    for block = 1:num_blocks
        
        trails_for_this_block_distractor = (find(block_with_distractor_subject == block));
        all_RT_with_distractor_subject = RT_with_distractor_subject(trails_for_this_block_distractor);
        correct_RT_with_distractor_subject = all_RT_with_distractor_subject(all_RT_with_distractor_subject~=999);
        median_RT_with_distractor_block = median(correct_RT_with_distractor_subject);
        
        trails_for_this_block_nondistractor = (find(block_without_distractor_subject == block));
        all_RT_without_distractor_subject = RT_without_distractor_subject(trails_for_this_block_nondistractor);
        correct_RT_without_distractor_subject = all_RT_without_distractor_subject(all_RT_without_distractor_subject~=999);
        median_RT_without_distractor_block = median(correct_RT_without_distractor_subject);
        
        median_RT_1st(s,block) = median_RT_with_distractor_block;
        median_RT_2nd(s,block) = median_RT_without_distractor_block;
    end
end
if length(subject)~=1
    SE_withwithout_distractors = [nanstd(median_RT_1st)/sqrt(length(subject));nanstd(median_RT_2nd)/sqrt(length(subject));];
    mean_median_RT_withwithout_distractors = [nanmean(median_RT_1st);nanmean(median_RT_2nd)];
else
    mean_median_RT_withwithout_distractors = [median_RT_1st; median_RT_2nd];
end
figure;
plot(mean_median_RT_withwithout_distractors','-o','LineWidth',2);
if length(subject)~=1
    errorbar(mean_median_RT_withwithout_distractors',SE_withwithout_distractors','-o','LineWidth',2);
end
legend('1st','2nd')
title(['mean of the median RT 1st and 2nd one of the pair in each block for repeating trials'],'FontSize',14);
ylabel('RT(s)')
xlabel('block')
ylim([0.7,1.1])

%4
median_RT_with_distractors = zeros(length(subject),num_blocks);
median_RT_without_distractors = zeros(length(subject),num_blocks);
SE_withwithout_distractors = zeros(num_blocks,2);

subject_index_distractor = subject_index(intersect(trials_with_distractor,find(novel_or_repeating_trials==0)));
RT_with_distractor = RT_with_999_incorrect(intersect(trials_with_distractor,find(novel_or_repeating_trials==0)));
block_with_distractor = all_block(intersect(trials_with_distractor,find(novel_or_repeating_trials==0)));
subject_index_nodistractor = subject_index(intersect(trials_without_distractor,find(novel_or_repeating_trials==0)));
RT_without_distractor = RT_with_999_incorrect(intersect(trials_without_distractor,find(novel_or_repeating_trials==0)));
block_without_distractor = all_block(intersect(trials_without_distractor,find(novel_or_repeating_trials==0)));

for s = 1:length(subject)
    RT_with_distractor_subject = RT_with_distractor(subject_index_distractor==s);
    block_with_distractor_subject = block_with_distractor(subject_index_distractor==s);
    RT_without_distractor_subject = RT_without_distractor(subject_index_nodistractor==s);
    block_without_distractor_subject = block_without_distractor(subject_index_nodistractor==s);
    for block = 1:num_blocks
        
        trails_for_this_block_distractor = (find(block_with_distractor_subject == block));
        all_RT_with_distractor_subject = RT_with_distractor_subject(trails_for_this_block_distractor);
        correct_RT_with_distractor_subject = all_RT_with_distractor_subject(all_RT_with_distractor_subject~=999);
        median_RT_with_distractor_block = median(correct_RT_with_distractor_subject);
        
        trails_for_this_block_nondistractor = (find(block_without_distractor_subject == block));
        all_RT_without_distractor_subject = RT_without_distractor_subject(trails_for_this_block_nondistractor);
        correct_RT_without_distractor_subject = all_RT_without_distractor_subject(all_RT_without_distractor_subject~=999);
        median_RT_without_distractor_block = median(correct_RT_without_distractor_subject);
        
        median_RT_with_distractors(s,block) = median_RT_with_distractor_block;
        median_RT_without_distractors(s,block) = median_RT_without_distractor_block;
    end
end
if length(subject)~=1
    SE_withwithout_distractors = [nanstd(median_RT_with_distractors)/sqrt(length(subject));nanstd(median_RT_without_distractors)/sqrt(length(subject));];
    mean_median_RT_withwithout_distractors = [nanmean(median_RT_with_distractors);nanmean(median_RT_without_distractors)];
else
    mean_median_RT_withwithout_distractors = [median_RT_with_distractors; median_RT_without_distractors];
end
figure;
plot(mean_median_RT_withwithout_distractors','-o','LineWidth',2);
if length(subject)~=1
    errorbar(mean_median_RT_withwithout_distractors',SE_withwithout_distractors','-o','LineWidth',2);
end
legend('distractor','nondistractor')
title(['mean of the median RT with and without distractor in each block for repeating trials'],'FontSize',14);
ylabel('RT(s)')
xlabel('block')
ylim([0.7,1.1])

%5
acc_with_distractors = zeros(length(subject),num_blocks);
acc_without_distractors = zeros(length(subject),num_blocks);
SE_withwithout_distractors = zeros(num_blocks,2);

subject_index_distractor = subject_index(intersect(trials_with_distractor,find(novel_or_repeating_trials==0)));
response_with_distractor = response(intersect(trials_with_distractor,find(novel_or_repeating_trials==0)));
block_with_distractor = all_block(intersect(trials_with_distractor,find(novel_or_repeating_trials==0)));
subject_index_nodistractor = subject_index(intersect(trials_without_distractor,find(novel_or_repeating_trials==0)));
response_without_distractor = response(intersect(trials_without_distractor,find(novel_or_repeating_trials==0)));
block_without_distractor = all_block(intersect(trials_without_distractor,find(novel_or_repeating_trials==0)));

for s = 1:length(subject)
    response_with_distractor_subject = response_with_distractor(subject_index_distractor==s);
    block_with_distractor_subject = block_with_distractor(subject_index_distractor==s);
    response_without_distractor_subject = response_without_distractor(subject_index_nodistractor==s);
    block_without_distractor_subject = block_without_distractor(subject_index_nodistractor==s);
    for block = 1:num_blocks
        
        trails_for_this_block_distractor = (find(block_with_distractor_subject == block));
        all_response_with_distractor_subject = response_with_distractor_subject(trails_for_this_block_distractor);
        r_with_distractor_subject = all_response_with_distractor_subject(all_response_with_distractor_subject~=999);
        acc_with_distractor_block = length(find(r_with_distractor_subject==1))/length(r_with_distractor_subject);
        
        trails_for_this_block_nondistractor = (find(block_without_distractor_subject == block));
        all_response_without_distractor_subject = response_without_distractor_subject(trails_for_this_block_nondistractor);
        r_without_distractor_subject = all_response_without_distractor_subject(all_response_without_distractor_subject~=999);
        acc_without_distractor_block = length(find(r_without_distractor_subject==1))/length(r_without_distractor_subject);
        
        acc_with_distractors(s,block) = acc_with_distractor_block;
        acc_without_distractors(s,block) = acc_without_distractor_block;
    end
end
if length(subject)~=1
    SE_withwithout_distractors = [std(acc_with_distractors)/sqrt(length(subject));std(acc_without_distractors)/sqrt(length(subject));];
    mean_median_RT_withwithout_distractors = [mean(acc_with_distractors);mean(acc_without_distractors)];
else
    mean_median_RT_withwithout_distractors = [acc_with_distractors; acc_without_distractors];
end
figure;
plot(mean_median_RT_withwithout_distractors','-o','LineWidth',2);
if length(subject)~=1
    errorbar(mean_median_RT_withwithout_distractors',SE_withwithout_distractors','-o','LineWidth',2);
end
legend('distractor','nondistractor')
title(['accuracy for trials with and without distractor in each block'],'FontSize',14);
ylabel('RT(s)')
xlabel('block')

%6
SE_of_median_across_template=[];
median_across_template=[];
subjAcc_across_template=[];
for block = 1:num_blocks
    
    all_array_this_block = all_array(all_block==block);
    novel_or_repeating_trials_this_block = novel_or_repeating_trials(all_block==block);
    RT_with_999_incorrect_this_block = RT_with_999_incorrect(all_block==block);
    subject_index_this_block = subject_index(all_block==block);
    with_or_without_distractor_trials_this_block = with_or_without_distractor_trials(all_block==block);
    first_or_second_trials_this_block = first_or_second_trials(all_block==block);
    
    array_in_each_condition_type{1} = [1,3,5,7];
    array_in_each_condition_type{2} = [2,4,10,12];
    array_in_each_condition_type{3} = [9,11,13,15];
    array_in_each_condition_type{4} = [6,8,14,16;];
    %novel without distractor
    array_in_each_condition_type{5} = [1:max(all_array)];
    %novel with distractor
    array_in_each_condition_type{6} = [1:max(all_array)];
    %novel 1st
    array_in_each_condition_type{7} = [1:max(all_array)];
    %novel 2nd
    array_in_each_condition_type{8} = [1:max(all_array)];
    %novel 1 without distractor
    array_in_each_condition_type{9} = [1:max(all_array)];
    %novel 1 with distractor
    array_in_each_condition_type{10} = [1:max(all_array)];
    %novel 2 without distractor
    array_in_each_condition_type{11} = [1:max(all_array)];
    %novel 2 with distractor
    array_in_each_condition_type{12} = [1:max(all_array)];
%     %real distractor 1 only (b6-10)
%     array_in_each_condition_type{13} = [9,11,13,15];
%     %real distractor 2 only (b6-10)
%     array_in_each_condition_type{14} = [6,8,14,16;];
    for condition_types = 1:size(array_in_each_condition_type,2)
        
        arrays = array_in_each_condition_type{condition_types};
        trials_for_these_arrays=[];
        for a = 1:length(arrays)
            if condition_types <= 4
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==0)'];
            elseif condition_types == 5
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & with_or_without_distractor_trials_this_block==0)'];
            elseif condition_types == 6
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & with_or_without_distractor_trials_this_block==1)'];
            elseif condition_types == 7
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & first_or_second_trials_this_block==1)'];
            elseif condition_types == 8
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & first_or_second_trials_this_block==2)'];
            elseif condition_types == 9
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & first_or_second_trials_this_block==1 & with_or_without_distractor_trials_this_block==0)'];
            elseif condition_types == 10
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & first_or_second_trials_this_block==1 & with_or_without_distractor_trials_this_block==1)'];
            elseif condition_types == 11
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & first_or_second_trials_this_block==2 & with_or_without_distractor_trials_this_block==0)'];
            elseif condition_types == 12
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & first_or_second_trials_this_block==2 & with_or_without_distractor_trials_this_block==1)'];
            elseif condition_types == 13 || condition_types == 14
                trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & with_or_without_distractor_trials_this_block==1 & novel_or_repeating_trials_this_block==0)'];
            end
        end
        RT_for_these_arrays_median = RT_with_999_incorrect_this_block(trials_for_these_arrays);
        subject_for_these_arrays_median = subject_index_this_block(trials_for_these_arrays);
        
        for n = 1:length(subject)
            RT_for_these_arrays_subject = RT_for_these_arrays_median(subject_for_these_arrays_median==n);
            
            RT_for_these_arrays_subject_median = RT_for_these_arrays_subject(RT_for_these_arrays_subject~=999);
            median_RT_for_these_arrays_subject = median(RT_for_these_arrays_subject_median);
            
            median_RT_for_these_arrays(n) = median_RT_for_these_arrays_subject;
            accuracy_for_these_arrays(n) = length(find(RT_for_these_arrays_subject~=999))/length(RT_for_these_arrays_subject);
        end
        
        median_across_template{condition_types,block} = median_RT_for_these_arrays;
        subjAcc_across_template{condition_types,block} = accuracy_for_these_arrays;
    end
end
medianRT = cell2mat(median_across_template);
save TASC6_medianRT_all_conditions_aware.mat medianRT

if length(subject)~=1
    mean_of_median_across_template = cellfun(@nanmean,median_across_template)';
    SE_of_median_across_template = (cellfun(@nanstd,median_across_template)/sqrt(length(subject)))';
    mean_subjAcc_across_template = cell2mat(subjAcc_across_template); 
else
    mean_of_median_across_template = cell2mat(median_across_template)';
end

figure;
errorbar([5,6],[mean_of_median_across_template(5,1),mean_of_median_across_template(6,1)],[SE_of_median_across_template(6,1),SE_of_median_across_template(6,2)])
hold on
errorbar([5,6],[mean_of_median_across_template(5,5),mean_of_median_across_template(6,5)],[SE_of_median_across_template(6,1),SE_of_median_across_template(6,2)])
ylabel('RT(s)')
xlabel('block')
% ylim([0.55,0.9])
xlim([4.5,6.5])
legend('repeated','novel')
title(['RT for normal trials only in block 5 and 6'],'FontSize',14);

%1
% mean_of_median_across_template_nov12=[mean_of_median_across_template(:,[1:4]),mean_of_median_across_template(:,[7:8])];
% if length(subject)~=1
%     SE_of_median_across_template_nov12=[SE_of_median_across_template(:,[1:4]),SE_of_median_across_template(:,[7:8])];
% end
mean_of_median_across_template_nov12=[mean_of_median_across_template(:,[1:4]),mean_of_median_across_template(:,[5:6])];
if length(subject)~=1
    SE_of_median_across_template_nov12=[SE_of_median_across_template(:,[1:4]),SE_of_median_across_template(:,[5:6])];
end
figure;
plot(mean_of_median_across_template_nov12,'-o','LineWidth',2);
if length(subject)~=1
    errorbar(mean_of_median_across_template_nov12,SE_of_median_across_template_nov12,'-o','LineWidth',2);
else
    plot(mean_of_median_across_template_nov12,'-o','LineWidth',2);
end
ylabel('RT(s)')
xlabel('block')
% legend('N1','N2','D1','D2','Nov1','Nov2')
legend('N1','N2','D1','D2','Nov(N)','Nov(D)')
title(['mean of median RT for each stimulus type across template'],'FontSize',14);

figure;
new_mean_of_median_across_template(:,1) = mean([mean_of_median_across_template_nov12(:,1),mean_of_median_across_template_nov12(:,2)],2);
new_mean_of_median_across_template(:,2) = mean([mean_of_median_across_template_nov12(:,3),mean_of_median_across_template_nov12(:,4)],2);
new_mean_of_median_across_template(:,3) = mean_of_median_across_template_nov12(:,5);
new_mean_of_median_across_template(:,4) = mean_of_median_across_template_nov12(:,6);

plot(new_mean_of_median_across_template,'-o','LineWidth',2);
if length(subject)~=1
    new_SE_of_median_across_template(:,1) = mean([SE_of_median_across_template_nov12(:,1),SE_of_median_across_template_nov12(:,2)],2);
    new_SE_of_median_across_template(:,2) = mean([SE_of_median_across_template_nov12(:,3),SE_of_median_across_template_nov12(:,4)],2);
    new_SE_of_median_across_template(:,3) = SE_of_median_across_template_nov12(:,5);
    new_SE_of_median_across_template(:,4) = SE_of_median_across_template_nov12(:,6);

    errorbar(new_mean_of_median_across_template,new_SE_of_median_across_template,'-o','LineWidth',2);
else
    plot(new_mean_of_median_across_template,'-o','LineWidth',2);
end
ylabel('RT(s)')
xlabel('block')
% legend('N1','N2','D1','D2','Nov1','Nov2')
legend('N','D','Nov(N)','Nov(D)')
title(['mean of median RT for each stimulus type across template'],'FontSize',14);


%ignore D at b1-5
mean_of_median_across_template_nov12=[mean_of_median_across_template(:,[1:2]),mean_of_median_across_template(:,[13:14]),mean_of_median_across_template(:,[5:6])];
if length(subject)~=1
    SE_of_median_across_template_nov12=[SE_of_median_across_template(:,[1:2]),SE_of_median_across_template(:,[13:14]),SE_of_median_across_template(:,[5:6])];
end
figure;
plot(mean_of_median_across_template_nov12,'-o','LineWidth',2);
if length(subject)~=1
    errorbar(mean_of_median_across_template_nov12,SE_of_median_across_template_nov12,'-o','LineWidth',2);
end
ylabel('RT(s)')
xlabel('block')
% legend('N1','N2','D1','D2','Nov1','Nov2')
legend('N1','N2','D1','D2','Nov(N)','Nov(D)')
title(['mean of median RT for each stimulus type across template'],'FontSize',14);









% %2
% mean_of_median_across_template_subtraction = [mean_of_median_across_template(:,1)-mean_of_median_across_template(:,7),...
%     mean_of_median_across_template(:,2)-mean_of_median_across_template(:,8),...
%     mean_of_median_across_template(:,3)-mean_of_median_across_template(:,7),...
%     mean_of_median_across_template(:,4)-mean_of_median_across_template(:,8)];
% figure;
% plot(mean_of_median_across_template_subtraction,'-o','LineWidth',2);
% ylabel('RT(s)')
% xlabel('block')
% legend('N1','N2','D1','D2')
% title(['mean of median RT for each stimulus type across template'],'FontSize',14);

% %3
% median_across_template_nd = median_across_template(1:4,:);
% median_across_template_nov1212 = [median_across_template(7,:);median_across_template(8,:);median_across_template(7,:);median_across_template(8,:)];
%
% median_across_template_sub = cellfun(@minus,median_across_template_nd,median_across_template_nov1212,'un',0);
% mean_median_across_template_sub = cellfun(@nanmean,median_across_template_sub)';
%
% figure;
% plot(mean_median_across_template_sub,'-o','LineWidth',2);
% ylabel('RT(s)')
% xlabel('block')
% legend('N1','N2','D1','D2')
% title(['mean of median RT for each stimulus type across template'],'FontSize',14);

% %7
% mean_of_median_across_template=[];
% SE_of_median_across_template=[];
% median_across_template=[];
% subjAcc_across_template=[];
% for block = 1:num_blocks
%     all_array_this_block = all_array(all_block==block);
%     novel_or_repeating_trials_this_block = novel_or_repeating_trials(all_block==block);
%     RT_with_999_incorrect_this_block = RT_with_999_incorrect(all_block==block);
%     subject_index_this_block = subject_index(all_block==block);
%     with_or_without_distractor_trials_this_block = with_or_without_distractor_trials(all_block==block);
%
%     array_in_each_condition_type{1} = [1,3];
%     array_in_each_condition_type{2} = [5,7];
%     array_in_each_condition_type{3} = [2,4];
%     array_in_each_condition_type{4} = [10,12];
%     array_in_each_condition_type{5} = [9,11];
%     array_in_each_condition_type{6} = [13,15];
%     array_in_each_condition_type{7} = [6,8];
%     array_in_each_condition_type{8} = [14,16;];
%     array_in_each_condition_type{9} = [1:max(all_array)];
%     array_in_each_condition_type{10} = [1:max(all_array)];
%     for condition_types = 1:size(array_in_each_condition_type,2)
%         arrays = array_in_each_condition_type{condition_types};
%         trials_for_these_arrays=[];
%         for a = 1:length(arrays)
%             if condition_types <= 8
%                 trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==0)'];
%             elseif condition_types == 9
%                 trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & with_or_without_distractor_trials_this_block==0)'];
%             elseif condition_types == 10
%                 trials_for_these_arrays = [trials_for_these_arrays,find(all_array_this_block==arrays(a) & novel_or_repeating_trials_this_block==1 & with_or_without_distractor_trials_this_block==1)'];
%             end
%         end
%         RT_for_these_arrays_median = RT_with_999_incorrect_this_block(trials_for_these_arrays);
%         subject_for_these_arrays_median = subject_index_this_block(trials_for_these_arrays);
%
%         for n = 1:length(subject)
%             RT_for_these_arrays_subject = RT_for_these_arrays_median(subject_for_these_arrays_median==n);
%
%             RT_for_these_arrays_subject_median = RT_for_these_arrays_subject(RT_for_these_arrays_subject~=999);
%             median_RT_for_these_arrays_subject = median(RT_for_these_arrays_subject_median);
%
%             median_RT_for_these_arrays(n) = median_RT_for_these_arrays_subject;
%             accuracy_for_these_arrays(n) = length(find(RT_for_these_arrays_subject~=999))/length(RT_for_these_arrays_subject);
%         end
%
%         median_across_template{condition_types,block} = median_RT_for_these_arrays;
%         subjAcc_across_template{condition_types,block} = accuracy_for_these_arrays;
%     end
% end
% if length(subject)~=1
%     mean_of_median_across_template = cellfun(@nanmean,median_across_template)';
%     SE_of_median_across_template = (cellfun(@std,median_across_template)/sqrt(length(subject)))';
% else
%     mean_of_median_across_template = cell2mat(median_across_template)';
% end
%
% figure;
% plot(mean_of_median_across_template,'-o','LineWidth',2);
% if length(subject)~=1
%     errorbar(mean_of_median_across_template,SE_of_median_across_template,'-o','LineWidth',2);
% end
% ylabel('RT(s)')
% xlabel('block')
% legend('N1(N)','N1(D)','(N)N2','(D)N2','D1(N)','D1(D)','(N)D2','(D)D2','Novel(N)','Novel(D)')
% title(['mean of median RT for each stimulus type across template'],'FontSize',14);

