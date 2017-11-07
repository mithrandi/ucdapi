import React from 'react'
import Panel from 'react-bootstrap/lib/Panel'
import 'npm/bootstrap/dist/css/bootstrap.min.css'
import {bindActionCreators} from 'redux'
import {connect} from 'react-redux'

import UnicodeVersion from './components/UnicodeVersion'
import * as actions from './actions'

const Home = ({app, ucdapi, versionChanged}) => (
    <div className="container">
      <h1>Unicode® Character Database</h1>
      <UnicodeVersion version={app.get('version')}
                      versions={ucdapi.get('versions')}
                      versionChanged={versionChanged}/>
      <Panel>Hi mom!!</Panel>
    </div>
)

export default connect(
    state => state,
    dispatch => ({
        versionChanged: bindActionCreators(actions, dispatch).versionChanged
    }))(Home)
