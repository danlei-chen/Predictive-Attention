clear all
subject = [2,3,4,5,6,7,8,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42];
% subject = [13,16,20];
% subject = [2,3,4,5,6,7,8,10,11,12,15,17,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42];
% 1 only did one block
% 9 at chance
%14 some blocks at chance

block_to_look_at=6:10;
startBlock=block_to_look_at(1);
endBlock=block_to_look_at(end);

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
num_novel_arrays_total = num_novel_arrays_per_set*repetition_per_block*num_blocks;
num_arrays_total = num_repeating_arrays+num_novel_arrays_total;
num_trials_per_block = num_arrays_per_set*repetition_per_block;
total_num_of_stimuli = num_blocks*num_trials_per_block;

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
    coordinates_info(:,coordinates_info(2)+1) = repmat([subject_order_num],[coordinates_info(1),1]);
    coordinates_info(buffer_trials,:)=[];
    
    coordinates_info_all = [coordinates_info_all; coordinates_info];
end

% results order:
% 1trial, 2block, 3array, 4minimal_dist_between_2_mid_points
% 5line_width, 6line_length, 7stim_presentation_time, 8real_onset_time_relative_to_block_start_time
% 9score, 10RT, 11response, 12the_button_that_they_pressed, 13nondistractor_color, 14distractor_color

% coordinates_info: 1trial/2, 2block, 3array, 4target_orientation, ...
% 5-32all_x_coordinates_without_target, 33-60all_y_coordinates_without_target, ...
% 61-64target_x, 65-68target_y, 69-72distractor_x, 73-76distractor_y, 77-78this_target_mid_point,79this_is_a_novel_trial];

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

%%%%%%%%%%%%%%%%%%%
%1
SE_of_median_across_template=[];
corr_across_template=[];
subjAcc_across_template=[];
array_in_each_condition_type={};
for block = startBlock:endBlock
    all_array_this_block = all_array(all_block==block);
    novel_or_repeating_trials_this_block = novel_or_repeating_trials(all_block==block);
    RT_with_999_incorrect_this_block = RT_with_999_incorrect(all_block==block);
    subject_index_this_block = subject_index(all_block==block);
    with_or_without_distractor_trials_this_block = with_or_without_distractor_trials(all_block==block);
    first_or_second_trials_this_block = first_or_second_trials(all_block==block);
    
    array_in_each_condition_type{1} = [1,3;];%N1(N)
    array_in_each_condition_type{2} = [2,4;];%(N)N2
    array_in_each_condition_type{3} = [5,7;];%N1(D)
    array_in_each_condition_type{4} = [6,8;];%(N)D2
    array_in_each_condition_type{5} = [9,11;];%D1(N)
    array_in_each_condition_type{6} = [10,12;];%(D)N2
    array_in_each_condition_type{7} = [13,15;];%D1(D)
    array_in_each_condition_type{8} = [14,16;];%(D)D2
    array_in_each_condition_type{9} = [1,3,5,7,9,11,13,15;];%all 1
    array_in_each_condition_type{10} = [2,4,10,12,6,8,14,16;];%all 2
    array_in_each_condition_type{11} = [1:max(all_array)];%novel 1st
    array_in_each_condition_type{12} = [1:max(all_array)];%novel 2nd
    % do two arrays of the same pair at the same time
    for condition_types = 1:size(array_in_each_condition_type,2)/2+1
        if condition_types <= size(array_in_each_condition_type,2)/2
            arrays1 = array_in_each_condition_type{condition_types*2-1};
            arrays2 = array_in_each_condition_type{condition_types*2};
        end
        trials_for_these_arrays1=[];
        trials_for_these_arrays2=[];
        if condition_types <= 4
            for a = 1:length(arrays1)
                trials_for_these_arrays1 = [trials_for_these_arrays1,find(all_array_this_block==arrays1(a) & novel_or_repeating_trials_this_block==0)'];
            end
            for a = 1:length(arrays2)
                trials_for_these_arrays2 = [trials_for_these_arrays2,find(all_array_this_block==arrays2(a) & novel_or_repeating_trials_this_block==0)'];
            end
        elseif condition_types == 5
            for a = 1:length(arrays1)
                trials_for_these_arrays1 = [trials_for_these_arrays1,find(all_array_this_block==arrays1(a) & novel_or_repeating_trials_this_block==0 & first_or_second_trials_this_block==1)'];
            end
            for a = 1:length(arrays2)
                trials_for_these_arrays2 = [trials_for_these_arrays2,find(all_array_this_block==arrays2(a) & novel_or_repeating_trials_this_block==0 & first_or_second_trials_this_block==2)'];
            end
        elseif condition_types == 6
            for a = 1:length(arrays1)
                trials_for_these_arrays1 = [trials_for_these_arrays1,find(all_array_this_block==arrays1(a) & novel_or_repeating_trials_this_block==1 & first_or_second_trials_this_block==1)'];
            end
            for a = 1:length(arrays2)
                trials_for_these_arrays2 = [trials_for_these_arrays2,find(all_array_this_block==arrays2(a) & novel_or_repeating_trials_this_block==1 & first_or_second_trials_this_block==2)'];
            end
        elseif condition_types == 7 % boundary
            repeating_trials_this_block = find(novel_or_repeating_trials_this_block==0);
            for n = 1:length(repeating_trials_this_block)-1
                if first_or_second_trials_this_block(repeating_trials_this_block(n))==2 && first_or_second_trials_this_block(repeating_trials_this_block(n+1))==1 && subject_index_this_block(repeating_trials_this_block(n))==subject_index_this_block(repeating_trials_this_block(n+1))
                    trials_for_these_arrays1 = [trials_for_these_arrays1,repeating_trials_this_block(n)'];
                    trials_for_these_arrays2 = [trials_for_these_arrays2,repeating_trials_this_block(n+1)'];
                end
            end
        end
        RT_for_these_arrays_median1 = RT_with_999_incorrect_this_block(trials_for_these_arrays1);
        RT_for_these_arrays_median2 = RT_with_999_incorrect_this_block(trials_for_these_arrays2);
        subject_for_these_arrays_median1 = subject_index_this_block(trials_for_these_arrays1);
        subject_for_these_arrays_median2 = subject_index_this_block(trials_for_these_arrays2);
        
        for n = 1:length(subject)
            RT_for_these_arrays_subject1 = RT_for_these_arrays_median1(subject_for_these_arrays_median1==n);
            RT_for_these_arrays_subject2 = RT_for_these_arrays_median2(subject_for_these_arrays_median2==n);
            % only use the pair when both of them are correct asnwer
            RT_for_these_arrays_subject2(find(RT_for_these_arrays_subject1==999))=999;
            RT_for_these_arrays_subject1(find(RT_for_these_arrays_subject2==999))=999;
            
            RT_for_these_arrays_subject_median1 = RT_for_these_arrays_subject1(RT_for_these_arrays_subject1~=999);
            RT_for_these_arrays_subject_median2 = RT_for_these_arrays_subject2(RT_for_these_arrays_subject2~=999);
            if isempty(RT_for_these_arrays_subject_median1) || isempty(RT_for_these_arrays_subject_median2)
                correlation_RT_for_these_arrays_subject=0;
            else
                correlation_RT_for_these_arrays_subject = corr(RT_for_these_arrays_subject_median1,RT_for_these_arrays_subject_median2);
            end
            
            corr_RT_for_these_arrays(n) = correlation_RT_for_these_arrays_subject;
        end
        % 1:N1N2 2:N1D2 3:D1N2 4:D1D2 5:all rep1rep2 6:nov1nov2 7:boundary1boundary2 8:averaged rep1rep2
        corr_across_template{condition_types,find(block_to_look_at==block)} = corr_RT_for_these_arrays;

    end
    
    %add one condition to average each subject to getrep 1&2 in a different way
    mean_subj_this_condition_this_block=[];
    for n = 1:length(subject)
        this_subj_this_condition_this_block=[];
        for con = 1:4
            all_subj_this_condition_this_block = corr_across_template{con,find(block_to_look_at==block)};
            this_subj_this_condition_this_block = [this_subj_this_condition_this_block,all_subj_this_condition_this_block(n)];
        end
        mean_subj_this_condition_this_block=[mean_subj_this_condition_this_block,mean(this_subj_this_condition_this_block)];
    end
    % 1:N1N2 2:N1D2 3:D1N2 4:D1D2 5:all rep1rep2 6:nov1nov2 7:boundary1boundary2 8:averaged rep1rep2
    corr_across_template{condition_types+1,find(block_to_look_at==block)}=mean_subj_this_condition_this_block;

end

if length(subject)~=1 && length(block_to_look_at)~=1
    mean_of_corr_across_template = cellfun(@nanmean,corr_across_template)';
    SE_of_median_across_template = (cellfun(@std,corr_across_template)/sqrt(length(subject)))';
else
    mean_of_corr_across_template = cell2mat(corr_across_template)';
end

% 1:N1N2 2:N1D2 3:D1N2 4:D1D2
figure;
y=[mean(mean_of_corr_across_template(:,1),1),mean(mean_of_corr_across_template(:,2),1),mean(mean_of_corr_across_template(:,3),1),mean(mean_of_corr_across_template(:,4),1)];
stderr=[std(mean_of_corr_across_template(:,1),1)/sqrt(length(subject)), std(mean_of_corr_across_template(:,2),1)/sqrt(length(subject)), std(mean_of_corr_across_template(:,3),1)/sqrt(length(subject)), std(mean_of_corr_across_template(:,4),1)/sqrt(length(subject))];
x=[1:4];
bar(x,y);
hold on;
er = errorbar(x,y,stderr);
er.Color = [0 0 0];                            
er.LineStyle = 'none';  
%paired t-test
ttest_result=[];
[h,p] = ttest([cell2mat(corr_across_template(1,:))]',cell2mat(corr_across_template(2,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
[h,p] = ttest([cell2mat(corr_across_template(1,:))]',cell2mat(corr_across_template(3,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
[h,p] = ttest([cell2mat(corr_across_template(1,:))]',cell2mat(corr_across_template(4,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
[h,p] = ttest([cell2mat(corr_across_template(2,:))]',cell2mat(corr_across_template(3,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
[h,p] = ttest([cell2mat(corr_across_template(2,:))]',cell2mat(corr_across_template(4,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
[h,p] = ttest([cell2mat(corr_across_template(3,:))]',cell2mat(corr_across_template(4,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
disp('N1N2vsN1D1     N1N2vsD1N2     N1N2vsD1D2     N1D1vsD1N2     N1D1vsD1D2    D1N2vsD1D2')
disp(ttest_result')
hold on
line([1,1.8], [max(y+stderr)+0.03,max(y+stderr)+0.03], 'Color', 'black', 'LineWidth', 1)
text(1.3,max(y+stderr)+0.04,num2str(ttest_result(1)))
line([1,2.8], [max(y+stderr)+0.05,max(y+stderr)+0.05], 'Color', 'black', 'LineWidth', 1)
text(1.8,max(y+stderr)+0.06,num2str(ttest_result(2)))
line([1,3.8], [max(y+stderr)+0.09,max(y+stderr)+0.09], 'Color', 'black', 'LineWidth', 1)
text(2.3,max(y+stderr)+0.10,num2str(ttest_result(3)))
line([2,2.8], [max(y+stderr)+0.03,max(y+stderr)+0.03], 'Color', 'black', 'LineWidth', 1)
text(2.3,max(y+stderr)+0.04,num2str(ttest_result(4)))
line([2,3.8], [max(y+stderr)+0.07,max(y+stderr)+0.07], 'Color', 'black', 'LineWidth', 1)
text(2.8,max(y+stderr)+0.08,num2str(ttest_result(5)))
line([3,3.8], [max(y+stderr)+0.03,max(y+stderr)+0.03], 'Color', 'black', 'LineWidth', 1)
text(3.3,max(y+stderr)+0.04,num2str(ttest_result(6)))
hold off
set(gca, 'XTickLabel', {'N1N2' 'N1D2' 'D1N2' 'D1D2'});
ylim([-0.02 0.25]);
title(['pair type average for each pair type']);

%'averaged rep 1&2'(8), 'boundry'(7), 'nov 1&2'(6)
figure;
y=[mean(mean_of_corr_across_template(:,8),1),mean(mean_of_corr_across_template(:,7),1),mean(mean_of_corr_across_template(:,6),1)];
stderr=[std(mean_of_corr_across_template(:,8),1)/sqrt(length(subject)), std(mean_of_corr_across_template(:,7),1)/sqrt(length(subject)), std(mean_of_corr_across_template(:,6),1)/sqrt(length(subject))];
x=[1:3];
bar(x,y);
hold on;
er = errorbar(x,y,stderr);
er.Color = [0 0 0];                            
er.LineStyle = 'none';  
%paired t-test
ttest_result=[];
[h,p] = ttest([cell2mat(corr_across_template(8,:))]',cell2mat(corr_across_template(7,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
[h,p] = ttest([cell2mat(corr_across_template(6,:))]',cell2mat(corr_across_template(7,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
[h,p] = ttest([cell2mat(corr_across_template(8,:))]',cell2mat(corr_across_template(6,:))','Alpha',0.05);
ttest_result = [ttest_result;p];
disp('rep12 vs boundry     boundry vs nov12     rep12 vs nov12')
disp(ttest_result')
hold on
line([1,1.8], [max(y+stderr)+0.03,max(y+stderr)+0.03], 'Color', 'black', 'LineWidth', 1)
text(1.3,max(y+stderr)+0.04,num2str(ttest_result(1)))
line([2.2,3], [max(y+stderr)+0.03,max(y+stderr)+0.03], 'Color', 'black', 'LineWidth', 1)
text(2.3,max(y+stderr)+0.04,num2str(ttest_result(2)))
line([1,3], [max(y+stderr)+0.05,max(y+stderr)+0.05], 'Color', 'black', 'LineWidth', 1)
text(1.8,max(y+stderr)+0.06,num2str(ttest_result(3)))
hold off
set(gca, 'XTickLabel', {'pair type average 1&2', 'pair boundry', 'nov 1&2'});
ylim([-0.02 0.2]);

% % figure;
% if length(subject)~=1
%     errorbar(mean_of_corr_across_template,SE_of_median_across_template,'-o','LineWidth',2);
% else
%     plot(mean_of_corr_across_template,'-o','LineWidth',2);
% end
% ylabel('R')
% xlabel('block')
% legend('NN','ND','DN','DD', 'pooled 1&2','nov 1&2','boundry','averaged 1&2')
% title(['Correlation: between first and second of a pair type'],'FontSize',14);
% disp('      NN        ND        DN        DD    pooled 1&2   nov1&2   boundry   averaged 1&2')
% disp(mean(mean_of_corr_across_template,1))
% figure;
% bar(mean(mean_of_corr_across_template,1))
% set(gca, 'XTickLabel', {'NN' 'ND' 'DN' 'DD' 'pooled 1&2' 'nov1&2' 'boundry','averaged 1&2'})
% ylim([-0.02 0.2])
% %paired t-test
% ttest_result=[];
% for condition_types = 1:size(corr_across_template,1)
%     [h,p] = ttest([cell2mat(corr_across_template(condition_types,:))]',[zeros(1,size(cell2mat(corr_across_template(condition_types,:)),2))]','Alpha',0.05);
%     ttest_result = [ttest_result;h,p];
% end
% disp('      NN        ND        DN        DD    pooled 1&2   nov1&2   boundry   averaged 1&2')
% disp(ttest_result')
% figure;
% bar(ttest_result(:,2))
% set(gca, 'XTickLabel', {'NN' 'ND' 'DN' 'DD' 'pooled 1&2' 'nov1&2' 'boundry','averaged 1&2'})
% hold on
% plot(xlim,[0.05 0.05])
% 
% 
% %'averaged 1&2','boundry','nov1&2'
% figure;
% mean_mean_of_corr_across_template=mean(mean_of_corr_across_template,1);
% std_mean_of_corr_across_template=std(mean_of_corr_across_template,1);
% % bar([mean_mean_of_corr_across_template(8),mean_mean_of_corr_across_template(7),mean_mean_of_corr_across_template(6)])
% barwitherr([std_mean_of_corr_across_template(8),std_mean_of_corr_across_template(7),std_mean_of_corr_across_template(6)],...
%     [mean_mean_of_corr_across_template(8),mean_mean_of_corr_across_template(7),mean_mean_of_corr_across_template(6)])
% set(gca, 'XTickLabel', {'averaged 1&2','boundry','nov1&2' })
% ylim([-0.3 0.3])
% %paired t-test
% ttest_result=[];
% [h,p] = ttest([cell2mat(corr_across_template(8,:))]',cell2mat(corr_across_template(7,:))','Alpha',0.05);
% ttest_result = [ttest_result;h,p];
% [h,p] = ttest([cell2mat(corr_across_template(8,:))]',cell2mat(corr_across_template(6,:))','Alpha',0.05);
% ttest_result = [ttest_result;h,p];
% [h,p] = ttest([cell2mat(corr_across_template(7,:))]',cell2mat(corr_across_template(6,:))','Alpha',0.05);
% ttest_result = [ttest_result;h,p];
% disp('rep12 vs boundry     rep12 vs nov12     boundry vs nov12')
% disp(ttest_result')
% figure;
% bar(ttest_result(:,2))
% set(gca, 'XTickLabel', {'rep12 vs boundry', 'rep12 vs nov12','boundry vs nov12'})
% hold on
% plot(xlim,[0.05 0.05])
% 
% %'NN' 'ND' 'DN' 'DD'
% figure;
% mean_mean_of_corr_across_template=mean(mean_of_corr_across_template,1);
% std_mean_of_corr_across_template=std(mean_of_corr_across_template,1);
% bar(mean_mean_of_corr_across_template(1:4))
% barwitherr(std_mean_of_corr_across_template(1:4),mean_mean_of_corr_across_template(1:4))
% set(gca, 'XTickLabel', {'NN' 'ND' 'DN' 'DD' })
% ylim([-0.08 0.3])
% %paired t-test
% [p,anovatab]=anova1(mean_of_corr_across_template(:,1:4))


% get correlation for each unique pair
SE_of_median_across_template_unique=[];
corr_across_template_unique=[];
array_in_each_condition_type={};
for block = startBlock:endBlock
    all_array_this_block = all_array(all_block==block);
    novel_or_repeating_trials_this_block = novel_or_repeating_trials(all_block==block);
    RT_with_999_incorrect_this_block = RT_with_999_incorrect(all_block==block);
    subject_index_this_block = subject_index(all_block==block);
    with_or_without_distractor_trials_this_block = with_or_without_distractor_trials(all_block==block);
    first_or_second_trials_this_block = first_or_second_trials(all_block==block);
    
    array_in_each_condition_type{1} = 1;%N1(N) pair1
    array_in_each_condition_type{2} = 2;%(N)N2 pair1
    array_in_each_condition_type{3} = 3;%N1(N) pair2
    array_in_each_condition_type{4} = 4;%(N)N2 pair2
    array_in_each_condition_type{5} = 5;%N1(D) pair1
    array_in_each_condition_type{6} = 6;%(N)D2 pair1
    array_in_each_condition_type{7} = 7;%N1(D) pair2
    array_in_each_condition_type{8} = 8;%(N)D2 pair2
    array_in_each_condition_type{9} = 9;%D1(N) pair1
    array_in_each_condition_type{10} = 10;%(D)N2 pair1
    array_in_each_condition_type{11} = 11;%D1(N) pair2
    array_in_each_condition_type{12} = 12;%(D)N2 pair2
    array_in_each_condition_type{13} = 13;%D1(D) pair1
    array_in_each_condition_type{14} = 14;%(D)D2 pair1 
    array_in_each_condition_type{15} = 15;%D1(D) pair2
    array_in_each_condition_type{16} = 16;%(D)D2 pair2

    % do two arrays of the same pair at the same time
    for condition_types = 1:size(array_in_each_condition_type,2)/2
        if condition_types <= size(array_in_each_condition_type,2)/2
            arrays1 = array_in_each_condition_type{condition_types*2-1};
            arrays2 = array_in_each_condition_type{condition_types*2};
        end
        
        trials_for_these_arrays1=[];
        trials_for_these_arrays2=[];
        for a = 1:length(arrays1)
            trials_for_these_arrays1 = [trials_for_these_arrays1,find(all_array_this_block==arrays1(a) & novel_or_repeating_trials_this_block==0)'];
        end
        for a = 1:length(arrays2)
            trials_for_these_arrays2 = [trials_for_these_arrays2,find(all_array_this_block==arrays2(a) & novel_or_repeating_trials_this_block==0)'];
        end

        RT_for_these_arrays_median1 = RT_with_999_incorrect_this_block(trials_for_these_arrays1);
        RT_for_these_arrays_median2 = RT_with_999_incorrect_this_block(trials_for_these_arrays2);
        subject_for_these_arrays_median1 = subject_index_this_block(trials_for_these_arrays1);
        subject_for_these_arrays_median2 = subject_index_this_block(trials_for_these_arrays2);
        
        for n = 1:length(subject)
            RT_for_these_arrays_subject1 = RT_for_these_arrays_median1(subject_for_these_arrays_median1==n);
            RT_for_these_arrays_subject2 = RT_for_these_arrays_median2(subject_for_these_arrays_median2==n);
            % only use the pair when both of them are correct asnwer
            RT_for_these_arrays_subject2(find(RT_for_these_arrays_subject1==999))=999;
            RT_for_these_arrays_subject1(find(RT_for_these_arrays_subject2==999))=999;
            
            RT_for_these_arrays_subject_median1 = RT_for_these_arrays_subject1(RT_for_these_arrays_subject1~=999);
            RT_for_these_arrays_subject_median2 = RT_for_these_arrays_subject2(RT_for_these_arrays_subject2~=999);
            if isempty(RT_for_these_arrays_subject_median1) || isempty(RT_for_these_arrays_subject_median2)
                correlation_RT_for_these_arrays_subject=0;
            else
                correlation_RT_for_these_arrays_subject = corr(RT_for_these_arrays_subject_median1,RT_for_these_arrays_subject_median2);
            end
            
            corr_RT_for_these_arrays(n) = correlation_RT_for_these_arrays_subject;
        end
        % 1:N1N2(1) 2:N1N2(2) 3:N1D2(1) 4:N1D2(2) 5:D1N2(1) 6:D1N2(2) 7:D1D2(1) 8:D1D2(2)
        corr_across_template_unique{condition_types,find(block_to_look_at==block)} = corr_RT_for_these_arrays;

    end
    
%     %add one condition to average each subject to getrep 1&2 in a different way
%     mean_subj_this_condition_this_block=[];
%     for n = 1:length(subject)
%         this_subj_this_condition_this_block=[];
%         for con = 1:4
%             all_subj_this_condition_this_block = corr_across_template{con,find(block_to_look_at==block)};
%             this_subj_this_condition_this_block = [this_subj_this_condition_this_block,all_subj_this_condition_this_block(n)];
%         end
%         mean_subj_this_condition_this_block=[mean_subj_this_condition_this_block,mean(this_subj_this_condition_this_block)];
%     end
%     % 1:N1N2 2:N1D2 3:D1N2 4:D1D2 5:all rep1rep2 6:nov1nov2 7:boundary1boundary2 8:averaged rep1rep2
%     corr_across_template{condition_types+1,find(block_to_look_at==block)}=mean_subj_this_condition_this_block;

end

if length(subject)~=1 && length(block_to_look_at)~=1
    mean_of_corr_across_template_unique = cellfun(@nanmean,corr_across_template_unique)';
    SE_of_median_across_template_unique = (cellfun(@std,corr_across_template_unique)/sqrt(length(subject)))';
else
    mean_of_corr_across_template_unique = cell2mat(corr_across_template_unique)';
end

%sanity check
% 1:N1N2(1) 2:N1N2(2) 3:N1D2(1) 4:N1D2(2) 5:D1N2(1) 6:D1N2(2) 7:D1D2(1) 8:D1D2(2)
figure;
y=[mean(mean_of_corr_across_template_unique(:,1),1),mean(mean_of_corr_across_template_unique(:,2),1),mean(mean_of_corr_across_template_unique(:,3),1),mean(mean_of_corr_across_template_unique(:,4),1),mean(mean_of_corr_across_template_unique(:,5),1),mean(mean_of_corr_across_template_unique(:,6),1),mean(mean_of_corr_across_template_unique(:,7),1),mean(mean_of_corr_across_template_unique(:,8),1)];
stderr=[std(mean_of_corr_across_template_unique(:,1),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,2),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,3),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,4),1)/sqrt(length(subject)),...
    std(mean_of_corr_across_template_unique(:,5),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,6),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,7),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,8),1)/sqrt(length(subject))];
x=[1:8];
bar(x,y);
hold on;
er = errorbar(x,y,stderr);
er.Color = [0 0 0];                            
er.LineStyle = 'none';  
set(gca, 'XTickLabel', {'N1N2(1)' 'N1N2(2)' 'N1D2(1)' 'N1D2(2)' 'D1N2(1)' 'D1N2(2)' 'D1D2(1)' 'D1D2(2)'});
ylim([-0.02 0.25]);

figure;
y=[mean([mean_of_corr_across_template_unique(:,1);mean_of_corr_across_template_unique(:,2)]),...
    mean([mean_of_corr_across_template_unique(:,3);mean_of_corr_across_template_unique(:,4)]),...
    mean([mean_of_corr_across_template_unique(:,5);mean_of_corr_across_template_unique(:,6)]),...
    mean([mean_of_corr_across_template_unique(:,7);mean_of_corr_across_template_unique(:,8)])];
stderr_unque=[std([mean_of_corr_across_template_unique(:,1);mean_of_corr_across_template_unique(:,2)])/sqrt(length(subject)),...
        std([mean_of_corr_across_template_unique(:,3);mean_of_corr_across_template_unique(:,4)])/sqrt(length(subject)),...
        std([mean_of_corr_across_template_unique(:,5);mean_of_corr_across_template_unique(:,6)])/sqrt(length(subject)),...
        std([mean_of_corr_across_template_unique(:,7);mean_of_corr_across_template_unique(:,8)])/sqrt(length(subject))];
x=[1:4];
bar(x,y);
hold on;
er = errorbar(x,y,stderr_unque);
er.Color = [0 0 0];                            
er.LineStyle = 'none';  
set(gca, 'XTickLabel', {'N1N2' 'N1D2' 'D1N2' 'D1D2'});
ylim([-0.02 0.25]);
title(['unique pair average for each pair type']);

% 'unique pair average'(new) 'pooled rep 1&2'(5) 'pair type average'(8), 'boundry'(7), 'nov 1&2'(6)
figure;
y=[mean(mean(mean_of_corr_across_template_unique,1)),mean(mean_of_corr_across_template(:,5),1),mean(mean_of_corr_across_template(:,8),1),mean(mean_of_corr_across_template(:,7),1),mean(mean_of_corr_across_template(:,6),1)];
stderr=[mean(stderr_unque), std(mean_of_corr_across_template(:,5),1)/sqrt(length(subject)), std(mean_of_corr_across_template(:,8),1)/sqrt(length(subject)), std(mean_of_corr_across_template(:,7),1)/sqrt(length(subject)), std(mean_of_corr_across_template(:,6),1)/sqrt(length(subject))];
x=[1:5];
bar(x,y);
hold on;
er = errorbar(x,y,stderr);
er.Color = [0 0 0];                            
er.LineStyle = 'none';  
set(gca, 'XTickLabel', {'unique pair average' 'pooled rep 1&2' 'pair type average', 'pair boundry', 'nov 1&2'});
ylim([-0.02 0.2]);


% get correlation for image at 1st or 2nd position 
SE_of_median_across_template_unique=[];
corr_across_template_unique=[];
array_in_each_condition_type={};
for block = startBlock:endBlock
    all_array_this_block = all_array(all_block==block);
    novel_or_repeating_trials_this_block = novel_or_repeating_trials(all_block==block);
    RT_with_999_incorrect_this_block = RT_with_999_incorrect(all_block==block);
    subject_index_this_block = subject_index(all_block==block);
    with_or_without_distractor_trials_this_block = with_or_without_distractor_trials(all_block==block);
    first_or_second_trials_this_block = first_or_second_trials(all_block==block);
    
    array_in_each_condition_type{1} = 1;  %N1(N) pair1
    array_in_each_condition_type{2} = 3;  %N1(N) pair2
    array_in_each_condition_type{3} = 5;  %N1(D) pair1
    array_in_each_condition_type{4} = 7;  %N1(D) pair2
    array_in_each_condition_type{5} = 9;  %D1(N) pair1
    array_in_each_condition_type{6} = 11; %D1(N) pair2
    array_in_each_condition_type{7} = 13; %D1(D) pair1
    array_in_each_condition_type{8} = 15; %D1(D) pair2
    array_in_each_condition_type{9} = 2;  %(N)N2 pair1
    array_in_each_condition_type{10} = 4; %(N)N2 pair2
    array_in_each_condition_type{11} = 10;%(D)N2 pair1
    array_in_each_condition_type{12} = 12;%(D)N2 pair2
    array_in_each_condition_type{13} = 6; %(N)D2 pair1
    array_in_each_condition_type{14} = 8; %(N)D2 pair2
    array_in_each_condition_type{15} = 14;%(D)D2 pair1 
    array_in_each_condition_type{16} = 16;%(D)D2 pair2

    % do two arrays of the same pair at the same time
    for condition_types = 1:size(array_in_each_condition_type,2)/2
        if condition_types <= size(array_in_each_condition_type,2)/2
            arrays1 = array_in_each_condition_type{condition_types*2-1};
            arrays2 = array_in_each_condition_type{condition_types*2};
        end
        
        trials_for_these_arrays1=[];
        trials_for_these_arrays2=[];
        for a = 1:length(arrays1)
            trials_for_these_arrays1 = [trials_for_these_arrays1,find(all_array_this_block==arrays1(a) & novel_or_repeating_trials_this_block==0)'];
        end
        for a = 1:length(arrays2)
            trials_for_these_arrays2 = [trials_for_these_arrays2,find(all_array_this_block==arrays2(a) & novel_or_repeating_trials_this_block==0)'];
        end

        RT_for_these_arrays_median1 = RT_with_999_incorrect_this_block(trials_for_these_arrays1);
        RT_for_these_arrays_median2 = RT_with_999_incorrect_this_block(trials_for_these_arrays2);
        subject_for_these_arrays_median1 = subject_index_this_block(trials_for_these_arrays1);
        subject_for_these_arrays_median2 = subject_index_this_block(trials_for_these_arrays2);
        
        for n = 1:length(subject)
            RT_for_these_arrays_subject1 = RT_for_these_arrays_median1(subject_for_these_arrays_median1==n);
            RT_for_these_arrays_subject2 = RT_for_these_arrays_median2(subject_for_these_arrays_median2==n);
            % only use the pair when both of them are correct asnwer
            RT_for_these_arrays_subject2(find(RT_for_these_arrays_subject1==999))=999;
            RT_for_these_arrays_subject1(find(RT_for_these_arrays_subject2==999))=999;
            
            RT_for_these_arrays_subject_median1 = RT_for_these_arrays_subject1(RT_for_these_arrays_subject1~=999);
            RT_for_these_arrays_subject_median2 = RT_for_these_arrays_subject2(RT_for_these_arrays_subject2~=999);
            if isempty(RT_for_these_arrays_subject_median1) || isempty(RT_for_these_arrays_subject_median2)
                correlation_RT_for_these_arrays_subject=0;
            else
                correlation_RT_for_these_arrays_subject = corr(RT_for_these_arrays_subject_median1,RT_for_these_arrays_subject_median2);
            end
            
            corr_RT_for_these_arrays(n) = correlation_RT_for_these_arrays_subject;
        end
        % 1:N1N2(1) 2:N1N2(2) 3:N1D2(1) 4:N1D2(2) 5:D1N2(1) 6:D1N2(2) 7:D1D2(1) 8:D1D2(2)
        corr_across_template_unique{condition_types,find(block_to_look_at==block)} = corr_RT_for_these_arrays;

    end

end

if length(subject)~=1 && length(block_to_look_at)~=1
    mean_of_corr_across_template_unique = cellfun(@nanmean,corr_across_template_unique)';
    SE_of_median_across_template_unique = (cellfun(@std,corr_across_template_unique)/sqrt(length(subject)))';
else
    mean_of_corr_across_template_unique = cell2mat(corr_across_template_unique)';
end

%sanity check
% 1:N1(N) 2:N1(D) 3:D1(N) 4:D1(D) 5:(N)N2 6:(D)N2 7:(N)D2 8:(D)D2
figure;
y=[mean(mean_of_corr_across_template_unique(:,1),1),mean(mean_of_corr_across_template_unique(:,2),1),mean(mean_of_corr_across_template_unique(:,3),1),mean(mean_of_corr_across_template_unique(:,4),1),mean(mean_of_corr_across_template_unique(:,5),1),mean(mean_of_corr_across_template_unique(:,6),1),mean(mean_of_corr_across_template_unique(:,7),1),mean(mean_of_corr_across_template_unique(:,8),1)];
stderr=[std(mean_of_corr_across_template_unique(:,1),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,2),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,3),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,4),1)/sqrt(length(subject)),...
    std(mean_of_corr_across_template_unique(:,5),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,6),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,7),1)/sqrt(length(subject)), std(mean_of_corr_across_template_unique(:,8),1)/sqrt(length(subject))];
x=[1:8];
bar(x,y);
hold on;
er = errorbar(x,y,stderr);
er.Color = [0 0 0];
er.LineStyle = 'none';  
set(gca, 'XTickLabel', {'N1(N)' 'N1(D)' 'D1(N)' 'D1(D)' '(N)N2' '(D)N2' '(N)D2' '(D)D2'});
ylim([-0.1 0.25]);
