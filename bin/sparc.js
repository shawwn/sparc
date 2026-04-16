#!/usr/bin/env node
'use strict';
const { spawnSync } = require('child_process');
const path = require('path');
const script = process.platform === 'win32'
  ? path.join(__dirname, 'sparc.cmd')
  : path.join(__dirname, 'sparc');
const result = spawnSync(script, process.argv.slice(2), { stdio: 'inherit' });
process.exit(result.status ?? 1);
