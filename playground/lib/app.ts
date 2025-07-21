#!/usr/bin/env node
import * as cdk from 'aws-cdk-lib';
import { PlaygroundStack } from './playground-stack';

const app = new cdk.App();
new PlaygroundStack(app, 'PlaygroundStack', {});
