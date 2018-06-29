def makePass(action, passIdx, rerun=False):
    def doPass(dct):
        if ((not rerun) and ('PF_exitPipelineReason' not in dct)) or \
                (rerun and ('PF_exitPipelineReason' in dct and dct['PF_exitPipelineReason'][0] == 3)):
            if rerun:
                del dct['PF_exitPipelineReason']
            retval = action(dct)
            if retval != None:
                (msg, more) = retval
                dct['PF_exitPipelineReason'] = (passIdx, msg, more)
        return dct
    return doPass
