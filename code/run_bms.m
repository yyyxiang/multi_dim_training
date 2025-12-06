function run_bms(filename, phase)

%     For training: filename = 'output/training_model_evidence.csv'; phase = 'training';
%     For execution: filename = 'output/execution_loglikelihood.csv'; phase = 'execution';

    model_evidence = readmatrix(filename);
    [alpha,exp_r,xp,pxp,bor,g] = bms(model_evidence);
    pxp = array2table(pxp, 'VariableNames', {'Planning', 'Exploitation', 'Learning', 'Equity', 'Equality', 'General'});
    g = array2table(g, 'VariableNames', {'Planning', 'Exploitation', 'Learning', 'Equity', 'Equality', 'General'});
    writetable(pxp, ['output/', phase, '_protected_exceedance_probabilities.csv'], 'WriteVariableNames', true)
    writetable(g, ['output/', phase, '_posterior.csv'], 'WriteVariableNames', true)