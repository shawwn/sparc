#!/usr/bin/env node
'use strict';
const { spawnSync } = require('child_process');
const path = require('path');
const script = process.platform === 'win32'
  ? path.join(__dirname, 'arc.cmd')
  : path.join(__dirname, 'arc');
const result = spawnSync(script, process.argv.slice(2), { stdio: 'inherit', shell: process.platform === 'win32' });
process.exit(result.status ?? 1);
