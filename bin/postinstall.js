#!/usr/bin/env node
'use strict';
const { execSync } = require('child_process');
const cmd = process.platform === 'win32' ? 'bin\\setup.cmd' : 'bash bin/setup.sh';
execSync(cmd, { stdio: 'inherit' });
