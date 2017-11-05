import React from 'react'
import Panel from 'react-bootstrap/lib/Panel'
import 'npm/bootstrap/dist/css/bootstrap.min.css'
import {connect} from 'react-redux'

import UnicodeVersion from './components/UnicodeVersion'

const Home = ({app, ucdapi}) => (
    <div className="container">
      <h1>Unicode® Character Database</h1>
      <UnicodeVersion version={app.version} versions={ucdapi.get('versions')}/>
      <Panel>Hi mom!!</Panel>
    </div>
)

export default connect(state => state)(Home)
