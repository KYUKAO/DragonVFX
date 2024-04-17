using System;
using UnityEngine;
using UnityEngine.Playables;
using UnityEngine.ProBuilder.MeshOperations;

namespace LevelArtInitialLHQ
{
    [RequireComponent(typeof(PlayableDirector))]
    public class LoopTimeline : MonoBehaviour //脚本用于控制TimeLine播放，便于录制视频
    {
        private PlayableDirector director;
        public bool IsDebugging = false;

        void Start()
        {
            // 获取PlayableDirector组件
            director = GetComponent<PlayableDirector>();

            // 注册Timeline播放完毕时的回调函数
            director.stopped += OnPlayableDirectorStopped;

            director.playOnAwake = !IsDebugging;

        }

        private void Update()
        {
            if (IsDebugging && Input.GetKey(KeyCode.P))
            {
                director.time = 0f;
                director.Play();
            }
        }

        // 当PlayableDirector停止播放时调用
        private void OnPlayableDirectorStopped(PlayableDirector aDirector)
        {
            if (director == aDirector)
            {
                // 重新开始播放Timeline
                director.Play();
            }
        }

        void OnDestroy()
        {
            // 清理，避免内存泄漏
            director.stopped -= OnPlayableDirectorStopped;
        }
    }
}