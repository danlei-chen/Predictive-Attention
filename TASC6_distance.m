clear all
subject = [2,3,4,5,6,7,8,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,31,31,32,33,34,35,36,37,38,39,40,41,42];
% subject = [13,16,20];
% subject = [2,3,4,5,6,7,8,10,11,12,15,17,18,19,21,22,23,24,25,26,27,28,29,31,31,32,33,34,35,36,37,38,39,40,41,42];
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

sub=[];
item1_id=[];item2_id=[];
subj = repelem([1:length(subject)],8)';
item1_id = repmat([1:2:15]',[length(subject),1]);
item2_id = repmat([2:2:16]',[length(subject),1]);

%DISTANCE
targ1x=[];targ1y=[];targ2x=[];targ2y=[];dist1x=[];dist1y=[];dist2x=[];dist2y=[];
for n = 1:length(subject)
    for array = 1:16
        this_trial = find(all_array==array & subject_index == n & novel_or_repeating_trials == 0,1,'last');
        distMidX = coordinates_info_all(this_trial,136)-(coordinates_info_all(this_trial,136)-coordinates_info_all(this_trial,135))/2;
        distMidY = coordinates_info_all(this_trial,137)-(coordinates_info_all(this_trial,137)-coordinates_info_all(this_trial,138))/2;
        if mod(array,2)==1
            targ1x = [targ1x; coordinates_info_all(this_trial,141)];
            targ1y = [targ1y; coordinates_info_all(this_trial,142)];
            dist1x = [dist1x; distMidX];
            dist1y = [dist1y; distMidY];
        else
            targ2x = [targ2x; coordinates_info_all(this_trial,141)];
            targ2y = [targ2y; coordinates_info_all(this_trial,142)];
            dist2x = [dist2x; distMidX];
            dist2y = [dist2y; distMidY];
        end
    end
end

%RT
RT1=[];
RT2=[];
for n = 1:length(subject)
    for array = 1:16
        this_trial = find(all_array==array & subject_index == n & novel_or_repeating_trials == 0 & response==1);
        if mod(array,2)==1
            RT1 = [RT1;mean(RT(this_trial))];
        else
            RT2 = [RT2;mean(RT(this_trial))];
        end
    end
end

C = [subj,item1_id,item2_id,targ1x,targ1y,targ2x,targ2y,dist1x,dist1y,dist2x,dist2y,RT1,RT2];
cols = {'sub','item1_id','item2_id','targ1x','targ1y','targ2x','targ2y','dist1x','dist1y','dist2x','dist2y','RT1','RT2'};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% toss 0's
C(C==0)=nan;
% get various info
N = length(unique(C(:,strcmp('sub',cols))));
npairs = size(C,1);
condint = 1+double(~isnan(C(:,strcmp('dist1x',cols))))+2*double(~isnan(C(:,strcmp('dist2x',cols))));
condKey = {'NN','DN','ND','DD'};
conditions = condKey(condint)';

%preallocate i1=item 1, i2=item2
i1_tt_dist = nan(npairs,1);
i2_tt_dist = nan(npairs,1);
i1_dt_dist = nan(npairs,1);
i2_dt_dist = nan(npairs,1);
i1_td_dist = nan(npairs,1);
i2_td_dist = nan(npairs,1);
i1_dd_dist = nan(npairs,1);
i2_dd_dist = nan(npairs,1);

% loop over pairs, get distance measures
for nn = 1:npairs
    
    % calculate i2 measures
    i2_tt_dist(nn) = norm([C(nn,strcmp('targ1x',cols)) C(nn,strcmp('targ1y',cols))] - [C(nn,strcmp('targ2x',cols)) C(nn,strcmp('targ2y',cols))]);
    i2_dt_dist(nn) = norm([C(nn,strcmp('dist1x',cols)) C(nn,strcmp('dist1y',cols))] - [C(nn,strcmp('targ2x',cols)) C(nn,strcmp('targ2y',cols))]);
    i2_td_dist(nn) = norm([C(nn,strcmp('targ1x',cols)) C(nn,strcmp('targ1y',cols))] - [C(nn,strcmp('dist2x',cols)) C(nn,strcmp('dist2y',cols))]);
    i2_dd_dist(nn) = norm([C(nn,strcmp('dist1x',cols)) C(nn,strcmp('dist1y',cols))] - [C(nn,strcmp('dist2x',cols)) C(nn,strcmp('dist2y',cols))]);
    
    % calculate i2 measures from screen center
    i2_ttc_dist(nn) = norm([961,740] - [C(nn,strcmp('targ2x',cols)) C(nn,strcmp('targ2y',cols))])';
    i2_dtc_dist(nn) = norm([961,740] - [C(nn,strcmp('targ2x',cols)) C(nn,strcmp('targ2y',cols))])';
    i2_tdc_dist(nn) = norm([961,740] - [C(nn,strcmp('dist2x',cols)) C(nn,strcmp('dist2y',cols))])';
    i2_ddc_dist(nn) = norm([961,740] - [C(nn,strcmp('dist2x',cols)) C(nn,strcmp('dist2y',cols))])';
    
    % calculate i1 measures...
    othInds = find(ismember(C(:,strcmp('sub',cols)),C(nn,strcmp('sub',cols)))&~strcmp(conditions,conditions{nn}));
%     disp(num2str(othInds'))
    i1_tt_dist(nn) = nanmean(sqrt(sum(([C(nn,strcmp('targ1x',cols)) C(nn,strcmp('targ1y',cols))] - [C(othInds,strcmp('targ2x',cols)) C(othInds,strcmp('targ2y',cols))]).^2,2)));
    i1_dt_dist(nn) = nanmean(sqrt(sum(([C(nn,strcmp('dist1x',cols)) C(nn,strcmp('dist1y',cols))] - [C(othInds,strcmp('targ2x',cols)) C(othInds,strcmp('targ2y',cols))]).^2,2)));
    i1_td_dist(nn) = nanmean(sqrt(sum(([C(nn,strcmp('targ1x',cols)) C(nn,strcmp('targ1y',cols))] - [C(othInds,strcmp('dist2x',cols)) C(othInds,strcmp('dist2y',cols))]).^2,2)));
    i1_dd_dist(nn) = nanmean(sqrt(sum(([C(nn,strcmp('dist1x',cols)) C(nn,strcmp('dist1y',cols))] - [C(othInds,strcmp('dist2x',cols)) C(othInds,strcmp('dist2y',cols))]).^2,2)));
    
    % calculate i1 measures from screen center
    othInds = find(ismember(C(:,strcmp('sub',cols)),C(nn,strcmp('sub',cols)))&~strcmp(conditions,conditions{nn}));
    i1_ttc_dist(nn) = nanmean(sqrt(sum(([C(nn,strcmp('targ1x',cols)) C(nn,strcmp('targ1y',cols))] - [961,740]).^2,2)));
    i1_dtc_dist(nn) = nanmean(sqrt(sum(([C(nn,strcmp('dist1x',cols)) C(nn,strcmp('dist1y',cols))] - [961,740]).^2,2)));
    i1_tdc_dist(nn) = nanmean(sqrt(sum(([C(nn,strcmp('targ1x',cols)) C(nn,strcmp('targ1y',cols))] - [961,740]).^2,2)));
    i1_ddc_dist(nn) = nanmean(sqrt(sum(([C(nn,strcmp('dist1x',cols)) C(nn,strcmp('dist1y',cols))] - [961,740]).^2,2)));

end

% target-center
d1_RT = RT1(ismember(conditions,{'DD','DN'}));
d2_RT = RT2(ismember(conditions,{'DD','DN'}));
n1_RT = RT1(ismember(conditions,{'NN','ND'}));
n2_RT = RT2(ismember(conditions,{'NN','ND'}));

groupfactor = repelem(["D1","D2","N1","N2"],length(d1_RT));

d1_ttc_dist = i1_ttc_dist(ismember(conditions,{'DD','DN'}));
d2_ttc_dist = i2_ttc_dist(ismember(conditions,{'DD','ND'}));
n1_ttc_dist = i1_tdc_dist(ismember(conditions,{'NN','ND'}));
n2_ttc_dist = i2_dtc_dist(ismember(conditions,{'NN','DN'}));
figure;
gscatter([d1_RT;d2_RT;n1_RT;n2_RT],[d1_ttc_dist,d2_ttc_dist,n1_ttc_dist,n2_ttc_dist],groupfactor,'rkgb')
title('Target-Center (within-pair) Distance')
xlabel('RT')
ylabel('Distance')

R=corrcoef(d1_RT,d1_ttc_dist);
R=R(2);
hold on;
text(1,70,['D1: r=',num2str(R)],'Color','r')
R=corrcoef(d2_RT,d2_ttc_dist);
R=R(2);
hold on;
text(1,50,['D2: r=',num2str(R)],'Color','k')
R=corrcoef(n1_RT,n1_ttc_dist);
R=R(2);
hold on;
text(1,30,['N1: r=',num2str(R)],'Color','g')
R=corrcoef(n2_RT,n2_ttc_dist);
R=R(2);
hold on;
text(1,10,['N2: r=',num2str(R)],'Color','b')

P = polyfit(d1_RT',d1_ttc_dist,1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'r-.');
P = polyfit(d2_RT',d2_ttc_dist,1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'k-.');
P = polyfit(n1_RT',n1_ttc_dist,1);
yfit = P(1)*n1_RT+P(2);
hold on;
plot(n1_RT,yfit,'g-.');
P = polyfit(n2_RT',n2_ttc_dist,1);
yfit = P(1)*n2_RT+P(2);
hold on;
plot(n2_RT,yfit,'b-.');

% distractor-center
d1_tdc_dist = i1_ddc_dist(ismember(conditions,{'DD','DN'}));
d2_tdc_dist = i2_ddc_dist(ismember(conditions,{'DD','ND'}));
n1_tdc_dist = i1_dtc_dist(ismember(conditions,{'NN','ND'}));
n2_tdc_dist = i2_tdc_dist(ismember(conditions,{'NN','DN'}));

figure;
gscatter([d1_RT;d2_RT;n1_RT;n2_RT],[d1_tdc_dist,d2_tdc_dist,n1_tdc_dist,n2_tdc_dist],groupfactor,'rkgb')
title('Distactor-Center (within-pair) Distance')
xlabel('RT')
ylabel('Distance')

R=corrcoef(d1_RT,d1_tdc_dist);
R=R(2);
hold on;
text(1,70,['D1: r=',num2str(R)],'Color','r')
R=corrcoef(d2_RT,d2_tdc_dist);
R=R(2);
hold on;
text(1,50,['D2: r=',num2str(R)],'Color','k')
R=corrcoef(n1_RT,n1_tdc_dist);
R=R(2);
hold on;
text(1,30,['N1: r=',num2str(R)],'Color','g')
R=corrcoef(n2_RT,n2_tdc_dist);
R=R(2);
hold on;
text(1,10,['N2: r=',num2str(R)],'Color','b')

P = polyfit(d1_RT',d1_tdc_dist,1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'r-.');
P = polyfit(d2_RT',d2_tdc_dist,1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'k-.');
P = polyfit(n1_RT',n1_tdc_dist,1);
yfit = P(1)*n1_RT+P(2);
hold on;
plot(n1_RT,yfit,'g-.');
P = polyfit(n2_RT',n2_tdc_dist,1);
yfit = P(1)*n2_RT+P(2);
hold on;
plot(n2_RT,yfit,'b-.');

% target-target
d1_tt_dist = i1_tt_dist(ismember(conditions,{'DD','DN'}))';
d2_tt_dist = i2_tt_dist(ismember(conditions,{'DD','ND'}))';
n1_tt_dist = i1_tt_dist(ismember(conditions,{'NN','ND'}))';
n2_tt_dist = i2_tt_dist(ismember(conditions,{'NN','DN'}))';

figure;
gscatter([d1_RT;d2_RT;n1_RT;n2_RT],[d1_tt_dist,d2_tt_dist,n1_tt_dist,n2_tt_dist],groupfactor,'rkgb')
title('Target-Target (within-pair) Distance')
xlabel('RT')
ylabel('Distance')

R=corrcoef(d1_RT,d1_tt_dist);
R=R(2);
hold on;
text(1,70,['D1: r=',num2str(R)],'Color','r')
R=corrcoef(d2_RT,d2_tt_dist);
R=R(2);
hold on;
text(1,50,['D2: r=',num2str(R)],'Color','k')
R=corrcoef(n1_RT,n1_tt_dist);
R=R(2);
hold on;
text(1,30,['N1: r=',num2str(R)],'Color','g')
R=corrcoef(n2_RT,n2_tt_dist);
R=R(2);
hold on;
text(1,10,['N2: r=',num2str(R)],'Color','b')

P = polyfit(d1_RT',d1_tt_dist,1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'r-.');
P = polyfit(d2_RT',d2_tt_dist,1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'k-.');
P = polyfit(n1_RT',n1_tt_dist,1);
yfit = P(1)*n1_RT+P(2);
hold on;
plot(n1_RT,yfit,'g-.');
P = polyfit(n2_RT',n2_tt_dist,1);
yfit = P(1)*n2_RT+P(2);
hold on;
plot(n2_RT,yfit,'b-.');

% target-distractor
d1_td_dist = i1_td_dist(ismember(conditions,{'DD','DN'}))';
d2_td_dist = i2_td_dist(ismember(conditions,{'DD','ND'}))';
n1_td_dist = i1_td_dist(ismember(conditions,{'NN','ND'}))';
n2_td_dist = i2_td_dist(ismember(conditions,{'NN','DN'}))';

figure;
gscatter([d1_RT;d2_RT;n1_RT;n2_RT],[d1_td_dist,d2_td_dist,n1_td_dist,n2_td_dist],groupfactor,'rkgb')
title('Target-Distractor (within-pair) Distance')
xlabel('RT')
ylabel('Distance')

R=corrcoef(d1_RT,d1_td_dist);
R=R(2);
hold on;
text(1,70,['D1: r=',num2str(R)],'Color','r')
R=corrcoef(d2_RT,d2_td_dist);
R=R(2);
hold on;
text(1,50,['D2: r=',num2str(R)],'Color','k')
R=corrcoef(n1_RT,n1_td_dist);
R=R(2);
hold on;
text(1,30,['N1: r=',num2str(R)],'Color','g')
R=corrcoef(n2_RT,n2_td_dist);
R=R(2);
hold on;
text(1,10,['N2: r=',num2str(R)],'Color','b')

P = polyfit(d1_RT',d1_td_dist,1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'r-.');
P = polyfit(d2_RT',d2_td_dist,1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'k-.');
P = polyfit(n1_RT',n1_td_dist,1);
yfit = P(1)*n1_RT+P(2);
hold on;
plot(n1_RT,yfit,'g-.');
P = polyfit(n2_RT',n2_td_dist,1);
yfit = P(1)*n2_RT+P(2);
hold on;
plot(n2_RT,yfit,'b-.');

% distractor-target
d1_dt_dist = i1_dt_dist(ismember(conditions,{'DD','DN'}))';
d2_dt_dist = i2_dt_dist(ismember(conditions,{'DD','ND'}))';
n1_dt_dist = i1_dt_dist(ismember(conditions,{'NN','ND'}))';
n2_dt_dist = i2_dt_dist(ismember(conditions,{'NN','DN'}))';

figure;
gscatter([d1_RT;d2_RT;n1_RT;n2_RT],[d1_dt_dist,d2_dt_dist,n1_dt_dist,n2_dt_dist],groupfactor,'rkgb')
title('Distractor-Target (within-pair) Distance')
xlabel('RT')
ylabel('Distance')

R=corrcoef(d1_RT(~isnan(d1_dt_dist)),d1_dt_dist(~isnan(d1_dt_dist)));
R=R(2);
hold on;
text(1,70,['D1: r=',num2str(R)],'Color','r')
R=corrcoef(d2_RT(~isnan(d2_dt_dist)),d2_dt_dist(~isnan(d2_dt_dist)));
R=R(2);
hold on;
text(1,50,['D2: r=',num2str(R)],'Color','k')
R=corrcoef(n1_RT(~isnan(n1_dt_dist)),n1_dt_dist(~isnan(n1_dt_dist)));
R=R(2);
hold on;
text(1,30,['N1: r=',num2str(R)],'Color','g')
R=corrcoef(n2_RT(~isnan(n2_dt_dist)),n2_dt_dist(~isnan(n2_dt_dist)));
R=R(2);
hold on;
text(1,10,['N2: r=',num2str(R)],'Color','b')

P = polyfit(d1_RT(~isnan(d1_dt_dist))',d1_dt_dist(~isnan(d1_dt_dist)),1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'r-.');
P = polyfit(d2_RT(~isnan(d2_dt_dist))',d2_dt_dist(~isnan(d2_dt_dist)),1);
yfit = P(1)*d1_RT+P(2);
hold on;
plot(d1_RT,yfit,'k-.');
P = polyfit(n1_RT(~isnan(n1_dt_dist))',n1_dt_dist(~isnan(n1_dt_dist)),1);
yfit = P(1)*n1_RT+P(2);
hold on;
plot(n1_RT,yfit,'m-.');
P = polyfit(n2_RT(~isnan(n2_dt_dist))',n2_dt_dist(~isnan(n2_dt_dist)),1);
yfit = P(1)*n2_RT+P(2);
hold on;
plot(n2_RT,yfit,'b-.');

% distractor-distractor
d1_dd_dist = i1_dd_dist(ismember(conditions,{'DD','DN'}))';
d2_dd_dist = i2_dd_dist(ismember(conditions,{'DD','ND'}))';
n1_dd_dist = i1_dd_dist(ismember(conditions,{'NN','ND'}))';
n2_dd_dist = i2_dd_dist(ismember(conditions,{'NN','DN'}))';

figure;
gscatter([d1_RT;d2_RT;n1_RT;n2_RT],[d1_dd_dist,d2_dd_dist,n1_dd_dist,n2_dd_dist],groupfactor,'rkgb')
title('Distractor-Distactor (within-pair) Distance')
xlabel('RT')
ylabel('Distance')

R=corrcoef(d1_RT(~isnan(d1_dd_dist)),d1_dd_dist(~isnan(d1_dd_dist)));
R=R(2);
hold on;
text(1,70,['D1: r=',num2str(R)],'Color','r')
R=corrcoef(d2_RT(~isnan(d2_dd_dist)),d2_dd_dist(~isnan(d2_dd_dist)));
R=R(2);
hold on;
text(1,50,['D2: r=',num2str(R)],'Color','k')
R=corrcoef(n1_RT,n1_dd_dist);
R=R(2);
hold on;
text(1,30,['N1: r=',num2str(R)],'Color','g')
R=corrcoef(n2_RT,n2_dd_dist);
R=R(2);
hold on;
text(1,10,['N2: r=',num2str(R)],'Color','b')

P = polyfit(d1_RT(~isnan(d1_dd_dist))',d1_dd_dist(~isnan(d1_dd_dist)),1);
yfit = P(1)*d1_RT(~isnan(d1_dd_dist))+P(2);
hold on;
plot(d1_RT(~isnan(d1_dd_dist)),yfit,'r-.');
P = polyfit(d2_RT(~isnan(d2_dd_dist))',d2_dd_dist(~isnan(d2_dd_dist)),1);
yfit = P(1)*d2_RT(~isnan(d2_dd_dist))+P(2);
hold on;
plot(d2_RT(~isnan(d2_dd_dist)),yfit,'k-.');











