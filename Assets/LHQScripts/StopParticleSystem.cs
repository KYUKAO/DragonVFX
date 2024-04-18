using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Serialization;

namespace LevelArtInitialLHQ
{
    public class StopParticleSystem : MonoBehaviour
    {
        // 暴露的粒子系统参数
        public ParticleSystem StopParticle;
        public ParticleSystem PlayParticle;

        // 调用这个函数来停止粒子系统的播放
        public void StopParticlePlaying()
        {
            if (StopParticle != null)
            {
                StopParticle.Stop();
            }
        }

        public void ParticlePlay()
        {
            PlayParticle.Play();
        }
    }
}
