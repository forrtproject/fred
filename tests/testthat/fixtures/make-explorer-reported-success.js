#!/usr/bin/env node
// Regenerates explorer-reported-success.csv by running the fred-apps Explorer's own
// FReD.successCriteria.getOutcome() on records from its published data.
// Usage: node make-explorer-reported-success.js <fred-apps checkout> <output.csv> [--all]
// --all writes every record instead of up to five per combination of input values.
const fs = require('fs');
const path = require('path');
const vm = require('vm');

const [appsDir, outFile, flag] = process.argv.slice(2);
const context = {};
context.window = context; // The script expects the browser's global window
vm.createContext(context);
vm.runInContext(fs.readFileSync(path.join(appsDir, 'shared/js/success-criteria.js'), 'utf8'), context);
const criteria = context.FReD.successCriteria;
const { studies } = JSON.parse(fs.readFileSync(path.join(appsDir, 'data/explorer-data.json'), 'utf8'));

const seen = {};
const real = studies.filter(s => {
  const key = `${s.reported_success}|${s.result}`;
  seen[key] = (seen[key] || 0) + 1;
  return flag === '--all' || seen[key] <= 5;
});
// The published data has no missing values, so missing inputs are exercised separately
const synthetic = [
  [null, 'successful replication'], [null, 'failed replication'], [null, 'OS not significant'],
  ['', 'failed replication'], [null, null], ['', ''], ['mixed', null], ['informative failure', null]
].map(([reported_success, result]) => ({ id: 'synthetic', reported_success, result, outcomes: {} }));

const csv = v => (v === null || v === undefined ? 'NA' : `"${String(v).replace(/"/g, '""')}"`);
const rows = [...real, ...synthetic].map(s => {
  const { outcome, outcomeReport } = criteria.getOutcome(s, 'reported_success');
  return [csv(s.id), csv(s.reported_success), csv(s.result), csv(outcome), csv(outcomeReport)].join(',');
});
fs.writeFileSync(outFile, ['record,reported_success,result,explorer_outcome,explorer_outcome_report', ...rows].join('\n') + '\n');
