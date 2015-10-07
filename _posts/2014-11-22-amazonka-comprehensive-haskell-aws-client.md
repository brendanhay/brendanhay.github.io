---
layout: post
title: The Boredom of Authoring an API Client
categories:
  - AWS
  - Haskell
---

In my day job as a glorified System Administrator I have the opportunity to write infrastructure, services, and tooling in Haskell, where traditionally someone in my position might reach for the hammers labeled Perl, Python, or Ruby et al.

While the advantages are many and those can be left to another blog post - a recurring pain point where Haskell falls down is in what I would categorise as _mundane_ and _commercial_ library availability:

*   Mundane: offers little intellectual reward to the library author. For myself this is anything that includes vast swathes of (mostly) repititious serialisation code that cannot be nicely abstracted using something like `GHC.Generics`.

*   Commercial: Company X offers compelling service Y that you wish to utilise, of which there are officially supported client libraries in Java, .NET, Python, and Ruby.

Haskell offers plenty of mechanisms for limiting boilerplate and these generally work well in the face of uniformity (**See:** [pagerduty](http://hackage.haskell.org/package/pagerduty)), but faced with supporting an inconsistent API of sufficient scope, I hereby postulate both of the above categories will be satisfied and many shall wring their hands and despair.

<h3>Contents</h3>
* TOC
{:toc}

### Status Quo

As a concrete example, In early 2013 we decided to exclusively use [Amazon Web Services](http://aws.amazon.com/) for our entire infrastructure. Coupled with the fact that all of our backend/infrastructure related code is written in Haskell, the lack of comprehensive and consistent AWS libraries proved to be a problem.

Looking at the [AWS](http://hackage.haskell.org/packages/#cat:AWS) category on Hackage, the collectively supported services are:

*   Cloud Watch
*   Elastic Compute Cloud
*   Elastic Load Balancing
*   Elastic Transcoder
*   Identity and Access Management
*   Kinesis
*   Relational Database Service
*   Route53
*   Simple Database Service
*   Simple Email Service
*   Simple Notification Service
*   Simple Storage Service

In some of these implementations the supported feature set is incomplete and approximately 30 services from Amazon's total offering are not available at all.

This results in a subpar experience relative to [Python](https://github.com/boto/boto), [Ruby](https://github.com/aws/aws-sdk-ruby), [Java](https://github.com/aws/aws-sdk-java/), or [.NET](https://github.com/aws/aws-sdk-net/), for which there are official SDKs.

### A Comprehensive Haskell AWS Client

After coming to the realisation in late 2012 - early 2013, that there were no Haskell libraries supporting the services we wished to use, I went down the route of providing a stopgap solution so we could begin building our infrastructure without having to compromise our language choice. This yielded a code generation Frankenstein which crawled the AWS documentation HTML, available SOAP definitions, and XSDs to provide AutoScaling, EC2, IAM, S3, CloudWatch, Route53, and ELB bindings.

While this was immediately useful, the obvious inconsistencies arising from HTML brittleness along with public XSDs in particular being an apparently legacy artifact for most services, intertia set in and I was unable to continue utilising the above approach for expanding the library offerings.

Going back to the drawing board in mid 2013, I started working on implementing a more future proof and sustainable approach to providing a truly _comprehensive_ AWS SDK I could use for all my projects, both personal and professional.

The key enabler for this next approach was the discovery of the Amazon Service models, which are typically vendored with each of the official SDKs and provide a _reasonably_ well typed representation of each of the services, warts and all.

> **Aside:** the format of the service definitions has changed a couple of times and I've been forced to rewrite pieces of the generation code more than once due to oversight.

The end result is called [amazonka](https://github.com/brendanhay/amazonka), consisting of 43 different libraries covering all currently available non-preview AWS services.

The core libraries are:

*   `amazonka`: contains a monad transformer, send/receive, and pagination logic.
*   `amazonka-core`: contains serialisation/request/response logic, and common data types.

With the supported services being:

*   [amazonka-autoscaling](http://hackage.haskell.org/package/amazonka-autoscaling)
*   [amazonka-cloudformation](http://hackage.haskell.org/package/amazonka-cloudformation)
*   [amazonka-cloudfront](http://hackage.haskell.org/package/amazonka-cloudfront)
*   [amazonka-cloudsearch-domains](http://hackage.haskell.org/package/amazonka-cloudsearch-domains)
*   [amazonka-cloudsearch](http://hackage.haskell.org/package/amazonka-cloudsearch)
*   [amazonka-cloudtrail](http://hackage.haskell.org/package/amazonka-cloudtrail)
*   [amazonka-cloudwatch-logs](http://hackage.haskell.org/package/amazonka-cloudwatch-logs)
*   [amazonka-cloudwatch](http://hackage.haskell.org/package/amazonka-cloudwatch)
*   [amazonka-codedeploy](http://hackage.haskell.org/package/amazonka-codedeploy)
*   [amazonka-cognito-identity](http://hackage.haskell.org/package/amazonka-cognito-identity)
*   [amazonka-cognito-sync](http://hackage.haskell.org/package/amazonka-cognito-sync)
*   [amazonka-config](http://hackage.haskell.org/package/amazonka-config)
*   [amazonka-datapipeline](http://hackage.haskell.org/package/amazonka-datapipeline)
*   [amazonka-directconnect](http://hackage.haskell.org/package/amazonka-directconnect)
*   [amazonka-dynamodb](http://hackage.haskell.org/package/amazonka-dynamodb)
*   [amazonka-ec2](http://hackage.haskell.org/package/amazonka-ec2)
*   [amazonka-elasticache](http://hackage.haskell.org/package/amazonka-elasticache)
*   [amazonka-elasticbeanstalk](http://hackage.haskell.org/package/amazonka-elasticbeanstalk)
*   [amazonka-elastictranscoder](http://hackage.haskell.org/package/amazonka-elastictranscoder)
*   [amazonka-elb](http://hackage.haskell.org/package/amazonka-elb)
*   [amazonka-emr](http://hackage.haskell.org/package/amazonka-emr)
*   [amazonka-iam](http://hackage.haskell.org/package/amazonka-iam)
*   [amazonka-importexport](http://hackage.haskell.org/package/amazonka-importexport)
*   [amazonka-kinesis](http://hackage.haskell.org/package/amazonka-kinesis)
*   [amazonka-kms](http://hackage.haskell.org/package/amazonka-kms)
*   [amazonka-lambda](http://hackage.haskell.org/package/amazonka-lambda)
*   [amazonka-opsworks](http://hackage.haskell.org/package/amazonka-opsworks)
*   [amazonka-rds](http://hackage.haskell.org/package/amazonka-rds)
*   [amazonka-redshift](http://hackage.haskell.org/package/amazonka-redshift)
*   [amazonka-route53-domains](http://hackage.haskell.org/package/amazonka-route53-domains)
*   [amazonka-route53](http://hackage.haskell.org/package/amazonka-route53)
*   [amazonka-s3](http://hackage.haskell.org/package/amazonka-s3)
*   [amazonka-sdb](http://hackage.haskell.org/package/amazonka-sdb)
*   [amazonka-ses](http://hackage.haskell.org/package/amazonka-ses)
*   [amazonka-sns](http://hackage.haskell.org/package/amazonka-sns)
*   [amazonka-sqs](http://hackage.haskell.org/package/amazonka-sqs)
*   [amazonka-storagegateway](http://hackage.haskell.org/package/amazonka-storagegateway)
*   [amazonka-sts](http://hackage.haskell.org/package/amazonka-sts)
*   [amazonka-support](http://hackage.haskell.org/package/amazonka-support)
*   [amazonka-swf](http://hackage.haskell.org/package/amazonka-swf)

Some preliminary Hackage documentation is available [here](https://brendanhay.github.io/amazonka/).

In the following topics I'll briefly highlight some of the features and potentially contentious design decisions, and the reasoning behind them.

> **Note:** This is a preview release designed to gather feedback, and I've not used all of the services (for example Kinesis, or SNS) personally, which will no doubt result in issues regarding the de/serialisation of requests, responses, errors, and possibly tears.
> 
> I'm relying on the brave to offer up constructive feedback via [GitHub Issues](https://github.com/brendanhay/amazonka/issues) since the scope is too much for me to test in practice, alone.

### Liptstick on a Pig

Since the definitions appear to be generated from Java-style services, the corresponding AST and type information follows similar Object Oriented naming conventions and class level nesting.

This isn't particuarly nice to work with in a langauge like Haskell, as it results in alot of extraneous types. Libraries in various other languages provide the proverbial lipstick on a pig and alter the types in such a way to make them more consistent with the host language's semantics.

Despite these points, I feel the advantages of providing types which strictly implement the naming and structure of the AWS types makes it easier to follow along with the Amazon API reference, and the use of lenses in this case mitigates some of the annoyances relating to access and traversal.

The intent is to provide a more low-level interface which corresponds `1:1` with the actual API, and let people supply their own lipstick.

### Lenses and Roles

Amazon utilises a number of different de/serialisation mechanisms ranging from the venerable XML and JSON, to more esoteric querystring serialisation of datatypes, and I inevitably ran up against the prototypical `newtype` explosion when avoiding orphan instances due to the heavy usage of type classes.

The solution for this was divorcing the internal structure from the representation observed and manipulated by the user. This approach allows extensive use of newtype wrappers internally, to define non-orhpaned instances for types such as `NonEmpty`, `Natural`, `HashMap`, or `Bool`, but exposes the underlying type to the user and the wrapper is never needed outside the core library.

[Iso](http://hackage.haskell.org/package/lens/docs/Control-Lens-Iso.html)s are paired with lenses to hide the (un)wrapping of newtypes from the user.

[Roles](https://ghc.haskell.org/trac/ghc/wiki/Roles) are used to avoid the need to traverse structures such as `NonEmpty` or `HashMap` when converting between the internal and external representations.

Here is the `List` and `Map` newtype wrappers from `amazonka-core`:

<div class="highlight">

<pre><span class="c1">-- | List is used to define specialised JSON, XML, and Query instances for</span>
<span class="c1">-- serialisation and deserialisation.</span>
<span class="c1">--</span>
<span class="c1">-- The e :: Symbol over which list is parameterised</span>
<span class="c1">-- is used as the enclosing element name when serialising</span>
<span class="c1">-- XML or Query instances.</span>
<span class="kr">newtype</span> <span class="kt">List</span> <span class="p">(</span><span class="n">e</span> <span class="ow">::</span> <span class="kt">Symbol</span><span class="p">)</span> <span class="n">a</span> <span class="ow">=</span> <span class="kt">List</span> <span class="p">{</span> <span class="n">list</span> <span class="ow">::</span> <span class="p">[</span><span class="n">a</span><span class="p">]</span> <span class="p">}</span>
    <span class="kr">deriving</span> <span class="p">(</span><span class="kt">Eq</span><span class="p">,</span> <span class="kt">Ord</span><span class="p">,</span> <span class="kt">Show</span><span class="p">,</span> <span class="kt">Semigroup</span><span class="p">,</span> <span class="kt">Monoid</span><span class="p">)</span>

<span class="c1">-- Requires the RoleAnnotations GHC extension.</span>
<span class="kr">type</span> <span class="n">role</span> <span class="kt">List</span> <span class="n">phantom</span> <span class="n">representational</span>

<span class="nf">_List</span> <span class="ow">::</span> <span class="p">(</span><span class="kt">Coercible</span> <span class="n">a</span> <span class="n">b</span><span class="p">,</span> <span class="kt">Coercible</span> <span class="n">b</span> <span class="n">a</span><span class="p">)</span> <span class="ow">=></span> <span class="kt">Iso'</span> <span class="p">(</span><span class="kt">List</span> <span class="n">e</span> <span class="n">a</span><span class="p">)</span> <span class="p">[</span><span class="n">b</span><span class="p">]</span>
<span class="nf">_List</span> <span class="ow">=</span> <span class="n">iso</span> <span class="p">(</span><span class="n">coerce</span> <span class="o">.</span> <span class="n">list</span><span class="p">)</span> <span class="p">(</span><span class="kt">List</span> <span class="o">.</span> <span class="n">coerce</span><span class="p">)</span>

<span class="c1">-- | Map is used similarly to define specialised de/serialisation instances</span>
<span class="c1">-- and to allow coercion of the values of the HashMap, but not the Key.</span>
<span class="kr">newtype</span> <span class="kt">Map</span> <span class="n">k</span> <span class="n">v</span> <span class="ow">=</span> <span class="kt">Map</span>
    <span class="p">{</span> <span class="n">fromMap</span> <span class="ow">::</span> <span class="kt">HashMap</span> <span class="n">k</span> <span class="n">v</span>
    <span class="p">}</span> <span class="kr">deriving</span> <span class="p">(</span><span class="kt">Eq</span><span class="p">,</span> <span class="kt">Show</span><span class="p">,</span> <span class="kt">Monoid</span><span class="p">,</span> <span class="kt">Semigroup</span><span class="p">)</span>

<span class="kr">type</span> <span class="n">role</span> <span class="kt">Map</span> <span class="n">nominal</span> <span class="n">representational</span>

<span class="nf">_Map</span> <span class="ow">::</span> <span class="p">(</span><span class="kt">Coercible</span> <span class="n">a</span> <span class="n">b</span><span class="p">,</span> <span class="kt">Coercible</span> <span class="n">b</span> <span class="n">a</span><span class="p">)</span> <span class="ow">=></span> <span class="kt">Iso'</span> <span class="p">(</span><span class="kt">Map</span> <span class="n">k</span> <span class="n">a</span><span class="p">)</span> <span class="p">(</span><span class="kt">HashMap</span> <span class="n">k</span> <span class="n">b</span><span class="p">)</span>
<span class="nf">_Map</span> <span class="ow">=</span> <span class="n">iso</span> <span class="p">(</span><span class="n">coerce</span> <span class="o">.</span> <span class="n">fromMap</span><span class="p">)</span> <span class="p">(</span><span class="kt">Map</span> <span class="o">.</span> <span class="n">coerce</span><span class="p">)</span>
</pre>

</div>

And the usage from `Network.AWS.DynamoDB.Scan` in `amazonka-dynamodb`:

<div class="highlight">

<pre><span class="kr">data</span> <span class="kt">ScanResponse</span> <span class="ow">=</span> <span class="kt">ScanResponse</span>
    <span class="p">{</span> <span class="n">_srItems</span> <span class="ow">::</span> <span class="kt">List</span> <span class="s">"Items"</span> <span class="p">(</span><span class="kt">Map</span> <span class="kt">Text</span> <span class="kt">AttributeValue</span><span class="p">)</span>
    <span class="o">...</span>
    <span class="p">}</span> <span class="kr">deriving</span> <span class="p">(</span><span class="kt">Eq</span><span class="p">,</span> <span class="kt">Show</span><span class="p">)</span>

<span class="nf">srItems</span> <span class="ow">::</span> <span class="kt">Lens'</span> <span class="kt">ScanResponse</span> <span class="p">[</span><span class="kt">HashMap</span> <span class="kt">Text</span> <span class="kt">AttributeValue</span><span class="p">]</span>
<span class="nf">srItems</span> <span class="ow">=</span> <span class="n">lens</span> <span class="n">_srItems</span> <span class="p">(</span><span class="nf">\</span><span class="n">s</span> <span class="n">a</span> <span class="ow">-></span> <span class="n">s</span> <span class="p">{</span> <span class="n">_srItems</span> <span class="ow">=</span> <span class="n">a</span> <span class="p">})</span> <span class="o">.</span> <span class="n">_List</span>
</pre>

</div>

This hopefully illustrates the usefullness of the approach to convert between the two representations. The `srItems` lens above can be used to manipulate the field with the more friendly `[HashMap Text AttributeValue]` representation, and you can retain all of the benefits of wrapping newtypes at arbitrary depths internally.

The following links provide detailed explanations of Roles and their implementation:

*   POPL 2011 [Generative Type Abstraction and Type-level Computation](http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf) [PDF]
*   ICFP 2014 [Safe Coercions](http://www.cis.upenn.edu/~eir/papers/2014/coercible/coercible-ext.pdf) [PDF]
*   GHC specific [implementation notes](https://ghc.haskell.org/trac/ghc/wiki/RolesImplementation).

### Smart Constructors

Providing the minimum number of parameters to satisfy construction of a valid request is desirable for succinctness, as opposed to comprehensively specifying every field of the underlying record.

This simply involves defaulting any `Maybe a` or `Monoid` field types to their respective `Nothing` or `mempty`, and supplying a smart constructor which delineates only the required parameters.

For example the operation [CreateAutoScalingGroup](http://brendanhay.github.io/amazonka/amazonka-autoscaling/Network-AWS-AutoScaling-CreateAutoScalingGroup.html) contains 15 fields, most of which are optional, and can be constructed with the fewest parameters required to create a valid Auto Scaling Group, or modified using lenses to specify any additional values for the optional fields before sending.

<div class="highlight">

<pre><span class="nf">minimal</span> <span class="ow">::</span> <span class="kt">CreateAutoScalingGroup</span>
<span class="nf">minimal</span> <span class="ow">=</span> <span class="n">createAutoScalingGroup</span> <span class="s">"asg-name"</span> <span class="mi">1</span> <span class="mi">5</span> <span class="n">zones</span>
</pre>

</div>

Is equivalent to:

<div class="highlight">

<pre><span class="nf">comprehensive</span> <span class="ow">::</span> <span class="kt">CreateAutoScalingGroup</span>
<span class="nf">comprehensive</span> <span class="ow">=</span> <span class="n">minimal</span>
    <span class="o">&</span> <span class="n">casgLaunchConfigurationName</span> <span class="o">.~</span> <span class="kt">Nothing</span>
    <span class="o">&</span> <span class="n">casgInstanceId</span>              <span class="o">.~</span> <span class="kt">Nothing</span>
    <span class="o">&</span> <span class="n">casgDesiredCapacity</span>         <span class="o">.~</span> <span class="kt">Nothing</span>
    <span class="o">&</span> <span class="n">casgDefaultCooldown</span>         <span class="o">.~</span> <span class="kt">Nothing</span>
    <span class="o">&</span> <span class="n">casgLoadBalancerNames</span>       <span class="o">.~</span> <span class="n">mempty</span>
    <span class="o">&</span> <span class="n">casgHealthCheckType</span>         <span class="o">.~</span> <span class="kt">Nothing</span>
    <span class="o">&</span> <span class="n">casgHealthCheckGracePeriod</span>  <span class="o">.~</span> <span class="kt">Nothing</span>
    <span class="o">&</span> <span class="n">casgPlacementGroup</span>          <span class="o">.~</span> <span class="kt">Nothing</span>
    <span class="o">&</span> <span class="n">casgVPCZoneIdentifier</span>       <span class="o">.~</span> <span class="kt">Nothing</span>
    <span class="o">&</span> <span class="n">casgTerminationPolicies</span>     <span class="o">.~</span> <span class="n">mempty</span>
    <span class="o">&</span> <span class="n">casgTags</span>                    <span class="o">.~</span> <span class="n">mempty</span>
</pre>

</div>

### Type Families

Type families are used to associate service errors, signing algorithms, and responses with requests.

For example, issuing a [DescribeInstances](http://brendanhay.github.io/amazonka/amazonka-ec2/Network-AWS-EC2-DescribeInstances.html) request:

<div class="highlight">

<pre><span class="kr">import</span> <span class="nn">Network.AWS</span>
<span class="kr">import</span> <span class="nn">Network.AWS.EC2</span>

<span class="nf">main</span> <span class="ow">::</span> <span class="kt">IO</span> <span class="nb">()</span>
<span class="nf">main</span> <span class="ow">=</span> <span class="kr">do</span>
    <span class="n">env</span> <span class="ow"><-</span> <span class="n">getEnv</span> <span class="kt">NorthVirginia</span> <span class="kt">Discover</span>
    <span class="n">rs</span>  <span class="ow"><-</span> <span class="n">send</span> <span class="n">env</span> <span class="n">describeInstances</span>
    <span class="n">print</span> <span class="n">rs</span>
</pre>

</div>

Where `:type rs` is:

<div class="highlight">

<pre><span class="kt">Either</span> <span class="p">(</span><span class="kt">Er</span> <span class="p">(</span><span class="kt">Sv</span> <span class="kt">Describeinstances</span><span class="p">))</span> <span class="p">(</span><span class="kt">Rs</span> <span class="kt">Describeinstances</span><span class="p">)</span>
</pre>

</div>

Or more concretely:

<div class="highlight">

<pre><span class="kt">Either</span> <span class="kt">EC2Error</span> <span class="kt">DescribeInstancesResponse</span>
</pre>

</div>

This works well in practice provided the user is familiar with type families, due to the slightly more arcane type signatures and error messages.

### Documentation for Free

The service definitions contain reasonably comprehensive documentation which allows us to include the actual AWS reference alongside a majority of the fields and operations.

Take for example this response lens from [GenerateDataKey](http://brendanhay.github.io/amazonka/amazonka-kms/Network-AWS-KMS-GenerateDataKey.html):

<div class="highlight">

<pre><span class="c1">-- | Ciphertext that contains the wrapped key. You must store the blob</span>
<span class="c1">-- and encryption context so that the ciphertext can be decrypted.</span>
<span class="c1">-- You must provide both the ciphertext blob and the encryption context.</span>
<span class="nf">gdkrCiphertextBlob</span> <span class="ow">::</span> <span class="kt">Lens'</span> <span class="kt">GenerateDataKeyResponse</span> <span class="p">(</span><span class="kt">Maybe</span> <span class="kt">Base64</span><span class="p">)</span>
</pre>

</div>

Currently links and other markup are stripped, but in future I hope to convert it directly to Haddock and retain all of the supplied documentation in a fashion similar to the [official SDKs](https://github.com/aws/aws-sdk-net/blob/e2c3dfccea246ce9e0d23eace45cdca42a5eb6fd/AWSSDK_DotNet35/Amazon.AutoScaling/Model/AttachInstancesRequest.cs#L30).

### One Library per Service

To illustrate the large nature of the codebase, everybody's favourite productivity measurer `cloc` shows:

<div class="highlight">

<pre><span class="n">Language</span>        <span class="n">files</span>          <span class="n">blank</span>        <span class="n">comment</span>           <span class="n">code</span>
<span class="n">Haskell</span>          <span class="mi">1258</span>          <span class="mi">34462</span>          <span class="mi">78158</span>         <span class="mi">145314</span>
</pre>

</div>

Since you generally do not depend on every service simultaneously, forcing users to compile 140,000+ lines of code they are probably not interested in is pointless.

Despite the maintenance overheads, cabal versioning, and potential discovery problems, encapsulating the code along service boundaries results in a much better user experience.

### Conclusion

While generating code may not yield the same user friendliness as hand written code in every case, it seems to scale very well for this particular class of problem.

During the recent 2014 [AWS Invent](http://aws.amazon.com/new/reinvent) over 8 new services were announced, with Key Management Service, Lambda, Config, and CodeDeploy being available, effective immediately. I was able to support these services not long after announcement by running `amazonka-gen`:

<div class="highlight">

<pre><span class="n">make</span> <span class="n">clean</span>
<span class="n">make</span>
</pre>

</div>

Which was a nice validation of the approach.

Overall I'm happy with the current status and direction, despite there still being a large amount of work ahead to place Haskell on an equal footing with other langauges in regards to building Cloud services and infrastructure.

Some items that I've identified for the immediate roadmap are:

*   Some responses lack `required` field information, resulting in `Maybe a` for always-present fields. [Overrides](https://github.com/brendanhay/amazonka/blob/330e274ba6fe93a35d3aa6f21fd22ec21e16d2ea/gen/overrides/ec2.json#L28) need to be manually annotated.
*   Comprehensive testing and usage of all services.
*   Improved documentation parsing (retaining links as Haddock markup).
*   Additional hand written documentation about usage.
*   Implement waiters and retries according to the service specifications.
*   Examples.
*   Performance benchmarks and evaluation.
*   Utilise type-information for [string patterns](https://github.com/brendanhay/amazonka/blob/330e274ba6fe93a35d3aa6f21fd22ec21e16d2ea/gen/stage1/kinesis/2013-12-02.api.json#L567) and maximum list lengths.
*   Remove the dependency on conduit (it should be trivial to only depend on http-client).

> You can follow the reddit discussion [here](http://www.reddit.com/r/haskell/comments/2n2sgc/amazonka_comprehensive_amazon_web_services_sdk/).

**Example**: Here is a less trivial example which creates a KeyPair, SecurityGroup, authorises port 22 ingress, and launches an Instance:

<div class="highlight">

<pre><span class="cm">{-# LANGUAGE OverloadedStrings #-}</span>

<span class="kr">module</span> <span class="nn">Main</span> <span class="kr">where</span>

<span class="kr">import</span>           <span class="nn">Control.Applicative</span>
<span class="kr">import</span>           <span class="nn">Control.Lens</span>
<span class="kr">import</span>           <span class="nn">Control.Monad</span>
<span class="kr">import</span>           <span class="nn">Control.Monad.IO.Class</span>
<span class="kr">import</span>           <span class="nn">Control.Monad.Trans.AWS</span>
<span class="kr">import</span>           <span class="nn">Data.Monoid</span>
<span class="kr">import</span>           <span class="nn">Data.Text</span>                <span class="p">(</span><span class="kt">Text</span><span class="p">)</span>
<span class="kr">import</span> <span class="k">qualified</span> <span class="nn">Data.Text</span>                <span class="k">as</span> <span class="n">Text</span>
<span class="kr">import</span> <span class="k">qualified</span> <span class="nn">Data.Text.IO</span>             <span class="k">as</span> <span class="n">Text</span>
<span class="kr">import</span>           <span class="nn">Data.Time.Clock.POSIX</span>
<span class="kr">import</span>           <span class="nn">Network.AWS.EC2</span>

<span class="nf">main</span> <span class="ow">::</span> <span class="kt">IO</span> <span class="nb">()</span>
<span class="nf">main</span> <span class="ow">=</span> <span class="kr">do</span>
    <span class="n">ts</span>  <span class="ow"><-</span> <span class="kt">Text</span><span class="o">.</span><span class="n">pack</span> <span class="o">.</span> <span class="n">show</span> <span class="o"><$></span> <span class="n">getTimestamp</span>
    <span class="n">env</span> <span class="ow"><-</span> <span class="n">getEnv</span> <span class="kt">NorthVirginia</span> <span class="kt">Discover</span>
    <span class="n">r</span>   <span class="ow"><-</span> <span class="n">runAWST</span> <span class="n">env</span> <span class="o">$</span> <span class="kr">do</span>
        <span class="n">say</span> <span class="s">"Create KeyPair "</span> <span class="n">ts</span>
        <span class="n">k</span> <span class="ow"><-</span> <span class="n">send</span> <span class="p">(</span><span class="n">createKeyPair</span> <span class="n">ts</span><span class="p">)</span>

        <span class="kr">let</span> <span class="n">key</span>    <span class="ow">=</span> <span class="kt">Text</span><span class="o">.</span><span class="n">unpack</span> <span class="n">ts</span> <span class="o">++</span> <span class="s">".pem"</span>
            <span class="n">trusty</span> <span class="ow">=</span> <span class="s">"ami-5895242f"</span>

        <span class="n">say</span> <span class="s">"Writing KeyPair material to "</span> <span class="n">key</span>
        <span class="n">liftIO</span> <span class="p">(</span><span class="kt">Text</span><span class="o">.</span><span class="n">writeFile</span> <span class="n">key</span> <span class="p">(</span><span class="n">k</span> <span class="o">^.</span> <span class="n">ckprKeyMaterial</span><span class="p">))</span>

        <span class="n">say</span> <span class="s">"Create SecurityGroup "</span> <span class="n">ts</span>
        <span class="n">g</span> <span class="ow"><-</span> <span class="n">view</span> <span class="n">csgrGroupId</span> <span class="o"><$></span>
            <span class="n">send</span> <span class="p">(</span><span class="n">createSecurityGroup</span> <span class="n">ts</span> <span class="s">"amazonka-examples"</span><span class="p">)</span>

        <span class="n">say</span> <span class="s">"Authorizing SSH on SecurityGroup "</span> <span class="n">g</span>
        <span class="n">void</span> <span class="o">.</span> <span class="n">send</span> <span class="o">$</span> <span class="n">authorizeSecurityGroupIngress</span>
            <span class="o">&</span> <span class="n">asgiGroupId</span>    <span class="o">?~</span> <span class="n">g</span>
            <span class="o">&</span> <span class="n">asgiIpProtocol</span> <span class="o">?~</span> <span class="s">"tcp"</span>
            <span class="o">&</span> <span class="n">asgiFromPort</span>   <span class="o">?~</span> <span class="mi">22</span>
            <span class="o">&</span> <span class="n">asgiToPort</span>     <span class="o">?~</span> <span class="mi">22</span>
            <span class="o">&</span> <span class="n">asgiCidrIp</span>     <span class="o">?~</span> <span class="s">"0.0.0.0/22"</span>

        <span class="n">say</span> <span class="s">"Launching Instance with ImageId "</span> <span class="n">trusty</span>
        <span class="n">i</span> <span class="ow"><-</span> <span class="n">sendCatch</span> <span class="o">$</span> <span class="n">runInstances</span> <span class="n">trusty</span> <span class="mi">1</span> <span class="mi">1</span>
            <span class="o">&</span> <span class="n">riKeyName</span>          <span class="o">?~</span> <span class="n">ts</span>
            <span class="o">&</span> <span class="n">riInstanceType</span>     <span class="o">?~</span> <span class="kt">T2Micro</span>
            <span class="o">&</span> <span class="n">riSecurityGroupIds</span> <span class="o">.~</span> <span class="p">[</span><span class="n">g</span><span class="p">]</span>

        <span class="n">either</span> <span class="p">(</span><span class="nf">\</span><span class="n">e</span> <span class="ow">-></span> <span class="kr">do</span>
                   <span class="n">say</span> <span class="s">"Failed to Launch Instance "</span> <span class="n">e</span>
                   <span class="n">say</span> <span class="s">"Deleting SecurityGroup "</span> <span class="n">g</span>
                   <span class="n">void</span> <span class="o">.</span> <span class="n">send</span> <span class="o">$</span> <span class="n">deleteSecurityGroup</span> <span class="o">&</span> <span class="n">dsgGroupId</span> <span class="o">?~</span> <span class="n">g</span>
                   <span class="n">say</span> <span class="s">"Deleting KeyPair "</span> <span class="n">ts</span>
                   <span class="n">void</span> <span class="o">.</span> <span class="n">send</span> <span class="o">$</span> <span class="n">deleteKeyPair</span> <span class="n">ts</span>
                   <span class="n">throwAWSError</span> <span class="n">e</span><span class="p">)</span>
               <span class="n">return</span>
               <span class="n">i</span>

    <span class="n">print</span> <span class="n">r</span>

<span class="nf">getTimestamp</span> <span class="ow">::</span> <span class="kt">IO</span> <span class="kt">Integer</span>
<span class="nf">getTimestamp</span> <span class="ow">=</span> <span class="n">truncate</span> <span class="o"><$></span> <span class="n">getPOSIXTime</span>

<span class="nf">say</span> <span class="ow">::</span> <span class="kt">Show</span> <span class="n">a</span> <span class="ow">=></span> <span class="kt">Text</span> <span class="ow">-></span> <span class="n">a</span> <span class="ow">-></span> <span class="kt">AWS</span> <span class="nb">()</span>
<span class="nf">say</span> <span class="n">msg</span> <span class="ow">=</span> <span class="n">liftIO</span> <span class="o">.</span> <span class="kt">Text</span><span class="o">.</span><span class="n">putStrLn</span> <span class="o">.</span> <span class="n">mappend</span> <span class="n">msg</span> <span class="o">.</span> <span class="kt">Text</span><span class="o">.</span><span class="n">pack</span> <span class="o">.</span> <span class="n">show</span>
</pre>

</div>

It's worth mentioning that `async` and `wait` from the `lifted-async` library can be used to run the KeyPair and SecurityGroup related code above, concurrently.
