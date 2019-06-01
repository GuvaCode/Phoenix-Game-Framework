Newton Game Dynamics Pascal-Headertranslation
 Current SDK version 2.35 (Beta) (Revision #2)

Copyright (c) 2004-2012 by
Currently maintained by :
  Sascha Willems
Additional contributors :
  Stuart "Stucuk" Carey
  Executor
  Jon Walton
  Dominique Louis
Initial Author :
 S.Spasov (Sury)

About :
==============================================================================================
The file "NewtonImport.pas" contains the current translation of the header for the
Newton Game Dynamics Physics SDK (http://www.newtongamedynamics.com) and should work with
Delphi and Free Pascal. Other Pascal languages might work but were not tested
(except for VP, which was tested, but VP is discontinued).

"NewtonImport_JointLibrary" contains the current translation of the joint library with
different tpyes of joints. It was initially done by Exectutor and needs the dll
"JointLibrary.dll" from the Newton SDK.


Where to get :
==============================================================================================
The current version of this header will be avaliable from either http://newton.delphigl.de or 
http://newton.freemapeditor.com/ .

It's usually updated when the SDK itself is updated, but there may also be some out-of-order
updates in case there are errors/problems with the header translation.


History (in reverse order for direct visibility of recent updates) :
==============================================================================================

  Changes on 01.04.2012 by Stuart Carey (SC)
   + Tidyed up the unit.
   + Merged my "Branch" of the header with Sascha's

  Changes on 14.02.2012 by Sascha Willems (SW)
   + Updated to 2.35
     - Added NewtonBodyApplyImpulseArray
     - Added NewtonCreateTreeCollisionFromMesh
     - Added NewtonMesApplyTransform
     - Added NewtonMeshPlaneClip


  Changes on 12.04.2011 by Sascha Willems (SW)
   + Fixed paramters for NewtonMaterialGetContactForce
   + Fixed paramters for NewtonMaterialGetContactPositionAndNormal
   + Fixed paramters for NewtonMaterialGetContactTangentDirections

  Changes on 04.04.2011 by Sascha Willems (SW)
   + Updated to 2.33
      - Added NewtonSceneSetProxyUserData
      - Added NewtonSceneGetProxyUserData
      - Added NewtonMeshCreateFirstLayer
      - Added NewtonMeshCreateNextLayer
      - Added NewtonMeshConvexDecomposition
      - Added NewtonMeshVoronoiDecomposition
   + Added NewtonSceneGetFirstProxy (missed in previous release)
   + Added NewtonSceneGetNextProxy (missed in previous release)
   + Added NewtonUserJointSetFeedbackCollectorCallback (missed in previous release)
   + Fixed declaration for NewtonCollisionSetMaxBreakImpactImpulse
   + Fixed several minor spelling errors
   + Some cosmetic changes

  Changes on 21.02.2011 by Sascha Willems (SW)
   + Updated to 2.32
      - Added NewtonReadThreadPerformanceTicks
      - Changed NewtonWorldConvexCast to function returning int (was a procedure)

  Changes on 30.12.2010 by Sascha Willems (SW)
   + Updated to 2.29 Beta
      - Changed parameters for NewtonCollisionTreeRayCastCallback
      - Changed paramters for NewtonCreateBody (added Matrix)
      - Added NewtonHeightFieldRayCastCallback
      - Added NewtonHeightFieldSetUserRayCastCallback

  Changes on 06.12.2010 by Sascha Willems (SW)
   + Updated to 2.26 Beta
      - Added NewtonWorldFloatSize
      - Added NewtonGetMaxThreadsCount
      - Added NewtonMeshConvexHull
      - Added NewtonMeshFixTJoints
      - Added NewtonRemoveUnusedVertices
      - Added NewtonMeshGetNormalArray
      - Added NewtonMeshGetUV0Array
      - Added NewtonMeshGetUV1Array
      - Added NewtonMeshGetFirstPoint
      - Added NewtonMeshGetNextPoint
      - Added NewtonMeshGetPointIndex
      - Added NewtonMeshGetVertexIndexFromPoint
      - NewtonCreateCompoundCollisionFromMesh - Parameters updated
      - Removed NewtonMeshConvexApproximation
      - Renamed NewtonAddBodyImpulse to NewtonBodyAddImpulse*
      - Fixed declaration for NewtonCollisionForEachPolygonDo*
      - Replaced single datatype with float (could cause trouble with double precision)*
      - Fixed spelling errors in jointlibrary (SliderSetLimits and HingeSetLimits)*
      - Fixed parameters on NewtonMeshGetVertexStreams*
   + Updated joint library to 2.26 beta
      - Changed dll reference from JointLibrary.dll to dJointLibrary
      - Fixed spelling errors*

      (* thanks to ledahut for pointing me to these errors)

  Changes on 22.07.2010 by Sascha Willems (SW)
   + Updated to 2.23 Beta

  Changes on 04.03.2010 by Sascha Willems (SW)
   + Fixed parameter list for NewtonMeshGetVertexStreams (uv0 and uv1 instead of uv)
   + Fixed parameter list for NewtonMeshGetIndirectVertexStreams (uv0 and uv1 instead of uv)
  
  Changes on 31.12.2009 by Sascha Willems (SW)
   + Reverted back to old format, using only one file (NewtonImport.pas instead of three)
   + Fixed a wrong dll name in the joint library
   - Note : No header changes in 2.16
  
  Changes on 29.12.2009 by Sascha Willems (SW)
   + Updated to 2.15 Beta
      - NewtonIslandUpdate - Added parameter "world"
	  - NewtonDestroyBodyByExeciveForce - Added const for parameter "contact"
	  - NewtonCollisionDestructor - Added parameter "World"
	  - NewtonBodyIterator - Added parameter "userData"
	  - NewtonJointIterator - Added parameter "userData"
	  - NewtonWorldForEachJointDo - Added parameter "userData"
	  - NewtonWorldForEachBodyInAABBDo - Added parameter "userData"
      - NewtonMeshCreate - Added parameter "world"
	  - NewtonMeshCreatePlane - Added parameter "world", renamed "textureMatrix" to "textureMatrix0" and added parameter "textureMatrix1"
      - NewtonMeshCalculateOOBB - Changed dataytpe for "matrix" from Float to PFloat 	  
   + Updated joint library to 2.15 Beta
      - Added HingeGetJointAngle
      - Added HingeGetPinAxis
      - Added HingeCalculateJointOmega 	  
  
  Changes on 20.11.2009 by Stuart Carey (SC) 
   + Updated to 2.11 Beta (2.11 not released. Subject to change)

  Changes on 03.10.2009 by Stuart Carey (SC) 
   + Updated to 2.10 Beta

  Changes on 25.09.2009 by Stuart Carey (SC) 
   + Updated to 2.09 Beta
   + Added Executor's Joint Library Translation

  Changes on 09.03.2009 by Stuart Carey (SC) 
   + Converted to NGD 2.0 Beta

  Changes on 28.06.2006 by Sascha Willems 
   + NewtonBodyGetForceAndTorqueCallback	: Function added
   
  Updated to SDK 1.53 on 26.05.2006 by Sascha Willems
   - NewtonWorldRayCast			        : Changed parameters to new SDK

  Updated to SDK 1.52 on 13.03.2006 by Sascha Willems
   + NewtonWorldForEachBodyInAABBDo             : Function added
   - NewtonCreateConvexHull                     : Added consts to pointer params     

  Updated to SDK 1.5 on 02.01.2006 by Sascha Willems
   x NewtonWorldCollide                         : Removed (no longer in SDK)   
   + NewtonMaterialSetContactNormalAcceleration : Function added               
   + NewtonMaterialSetContactNormalDirection    : Function added               
   + NewtonCollisionPointDistance               : Function added               
   + NewtonCollisionClosestPoint                : Function added               
   + NewtonCollisionCollide                     : Function added               
   - NewtonBodyCoriolisForcesMode               : Corrected spelling (FPC)     
   - NewtonRagDollGetRootBone                   : Commented out (not in DLL)   
   x NewtonBodyGetTotalVolume                   : Removed (renamed in SDK)     
   + NewtonConvexCollisionCalculateVolume       : Function added
   + NewtonConvexCollisionCalculateInertial...  : Function added               
   + NewtonMaterialSetContinuousCollisionMode   : Function added               
   - NewtonUserJointSetRowMinimunFriction       : Corrected spelling           
   - NewtonUserJointSetRowMaximunFriction       : Corrected spelling           
   + NewtonCollisionCollideContinue             : Function added               
   + NewtonBodySetCentreOfMass                  : Function added               
   + NewtonBodyGetCentreOfMass                  : Function added               
   + NewtonUserJointSetRowSpringDamperAcce...   : Function added               
   x NewtonVehicleBalanceTires                  : Removed (no longer in SDK)   
   - NewtonGetBuoyancyPlane                     : Changed parameters to new SDK
   + NewtonSetPlatformArchitecture              : Function added               
   + NewtonCollisionMakeUnique                  : Function added               
   - NewtonVehicle*                             : Changed parameters to new SDK
   + NewtonUserJointAddGeneralRow               : Function added

  Changes on 13.04.2005 by Sascha Willems                                     
   - NewtonAllocMemory                           : Fixed declaration. Was declared    
                                               	  as procedure but should have been  
                                             	  a function returning a pointer.    
                                            	  Thx to Tux for pointing it out.    
  Changes on 03.04.2005 by Sascha Willems                                     
   - Symbol NEWTON_DOUBLE_PRECISION             : Define this when you want to use   
                                            	  Newton with the double precision   
                                           	  dll                                


Updated to SDK 1.31 on 09.01.2005 by Sascha Willems                           
   x NewtonUpVectorCallBack                     : Removed (no longer in SDK)         
   x NewtonUpVectorSetUserCallback        	: Removed (no longer in SDK)         
   - NewtonConstraintCreateUserJoint      	: Changed parameters to new SDK      
   x NewtonUserJointSetUserCallback       	: Removed (no longer in SDK)         
   + NewtonUserJointAddLinearRow;         	: Function added                     
   + NewtonUserJointAddAngularRow         	: Function added                     
   + NewtonUserJointSetRowMinimunFriction 	: Function added                     
   + NewtonUserJointSetRowMaximunFriction 	: Function added                     
   + NewtonUserJointSetRowAcceleration    	: Function added                     
   + NewtonUserJointSetRowStiffness       	: Function added                     
   + NewtonSetSolverModel                 	: Function added                     
   + NewtonSetFrictionModel               	: Function added                     
   + NewtonUserJointGetRowForce           	: Function addes                     
   + NewtonAddBodyImpulse                 	: Declaration fixed                  

 Fixes on 27.11.2004 by Sascha Willems                                         
   - NewtonGetBuoyancyPlane                     : globalSpaceMatrix changed to PFloat
   
 Fixes on 22/23/24/25.11.2004 by Sascha Willems                               
   - NewtonCollisionIterator                    : cdecl was missing                  
   - NewtonCreateSphere                   	: Fixed parameters                   
   - NewtonVehicleTireIsAirBorne          	: Corrected spelling error           
   - NewtonVehicleTireLostSideGrip        	: Corrected spelling error           
   - NewtonVehicleTireLostTraction        	: Corrected spelling error           
   - NewtonRagDollAddBone                 	: Fixed parameters                   
   - NewtonWorldCollide                   	: Added missing "const"s for params  
   - NewtonWorldRayFilterCallback         	: Fixed parameters and return value  
   - NewtonWorldRayCast                   	: Fixed parameters (removed normal)  
   - NewtonBodySetContinuousCollisionMode 	: Corrected spelling error           
   - NewtonConstraintCreateUniversal      	: Corrected spelling error           
   - NewtonJointSetStiffness              	: Corrected spelling error           
   + NewtonGetTimeStep                    	: Function added                     

  Non-Delphi compiler support added on 17.08.2004 by Dominique Louis           
  Newton 1.3 support added on 22.11.2004 by Jon Walton                         

  Conversion completed at 13.08.2004 by Sury

  Conversion started at 13.08.2004 by Sury

