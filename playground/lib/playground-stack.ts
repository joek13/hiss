import * as cdk from 'aws-cdk-lib';
import { Code, Function, Runtime } from 'aws-cdk-lib/aws-lambda';
import { Construct } from 'constructs';

export class PlaygroundStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props);

    new Function(this, 'PlaygroundFunction', {
      runtime: Runtime.PYTHON_3_13,
      handler: 'playground.handle',
      code: Code.fromAsset('./build/bundle.zip'),
    });
  }
}
