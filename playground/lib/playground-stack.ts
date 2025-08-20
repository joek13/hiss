import * as cdk from 'aws-cdk-lib';
import { Duration } from 'aws-cdk-lib';
import {
  Code,
  Function,
  FunctionUrlAuthType,
  Runtime,
} from 'aws-cdk-lib/aws-lambda';
import { Construct } from 'constructs';

export class PlaygroundStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props);

    const playgroundFn = new Function(this, 'PlaygroundFunction', {
      runtime: Runtime.PYTHON_3_13,
      handler: 'playground.handle',
      code: Code.fromAsset('./build/bundle.zip'),
      timeout: Duration.seconds(30),
    });

    playgroundFn.addFunctionUrl({
      authType: FunctionUrlAuthType.NONE, // Public endpoint.
      cors: {
        allowedOrigins: ['*'],
      },
    });
  }
}
